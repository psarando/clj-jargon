(ns clj-jargon.tickets
  (:use [clj-jargon.validations]
        [clj-jargon.init :only [anonymous-user-account override-user-account proxy-input-stream]]
        [clj-jargon.cart :only [temp-password]]
        [clj-jargon.item-info :only [file object-type]]
        [clj-jargon.item-ops :only [input-stream tcb]])
  (:require [clojure-commons.file-utils :as ft]
            [clojure.tools.logging :as log]
            [clojure-commons.error-codes :refer [ERR_NOT_WRITEABLE]]
            [slingshot.slingshot :refer [throw+]])
  (:import [java.io File]
           [org.irods.jargon.core.exception CatNoAccessException]
           [org.irods.jargon.core.pub IRODSAccessObjectFactory]
           [org.irods.jargon.core.connection IRODSAccount]
           [org.irods.jargon.ticket.packinstr TicketInp] 
           [org.irods.jargon.ticket.packinstr TicketCreateModeEnum] 
           [org.irods.jargon.ticket TicketServiceFactoryImpl
                                    TicketAdminService
                                    TicketAdminServiceImpl
                                    TicketClientSupport
                                    TicketClientOperations
                                    Ticket]))

(defn- ^IRODSAccount account-for-ticket
  [cm username]
  (condp = username
    (:username cm)
    (do (log/debug (str "Using existing account since '" username "' = '" (:username cm) "'"))
        (:irodsAccount cm))

    "anonymous"
    (anonymous-user-account cm)

    (do (log/debug (str "Creating temporary password for '" username "' which does not match '" (:username cm) "'"))
        (override-user-account cm username (temp-password cm username)))))

(defn- ^TicketAdminService ticket-admin-service
  "Creates an instance of TicketAdminService, which provides
   access to utility methods for performing operations on tickets.
   Probably doesn't need to be called directly."
  [cm username]
  (let [tsf (:ticketServiceFactory cm)
        user (account-for-ticket cm username)]
    (.instanceTicketAdminService tsf user)))

(defn- ^TicketClientOperations ticket-client-operations
  "Creates an instance of TicketAdminService, which provides
   access to utility methods for performing operations on tickets.
   Probably doesn't need to be called directly."
  ([cm]
   (ticket-client-operations cm (:username cm)))
  ([cm username]
   (let [tsf (:ticketServiceFactory cm)
         user (account-for-ticket cm username)]
     (.instanceTicketClientOperations tsf user))))

(defn set-ticket-options
  "Sets the optional settings for a ticket, such as the expiration date
   and the uses limit."
  [ticket-id ^TicketAdminService tas
   {:keys [byte-write-limit expiry file-write-limit uses-limit]}]
  (when byte-write-limit
    (.setTicketByteWriteLimit tas ticket-id byte-write-limit))
  (when expiry
    (.setTicketExpiration tas ticket-id expiry))
  (when file-write-limit
    (.setTicketFileWriteLimit tas ticket-id file-write-limit))
  (when uses-limit
    (.setTicketUsesLimit tas ticket-id uses-limit)))

(defn create-ticket
  [cm user fpath ticket-id & {:keys [rw-mode] :as ticket-opts}]
  (validate-path-lengths fpath)
  (let [tas        (ticket-admin-service cm user)
        read-mode  (if (= rw-mode :write) TicketCreateModeEnum/WRITE TicketCreateModeEnum/READ)
        new-ticket (.createTicket tas read-mode (file cm fpath) ticket-id)]
    (set-ticket-options ticket-id tas ticket-opts)
    new-ticket))

(defn modify-ticket
  [cm user ticket-id & {:as ticket-opts}]
  (set-ticket-options ticket-id (ticket-admin-service cm user) ticket-opts))

(defn delete-ticket
  "Deletes the ticket specified by ticket-id."
  [cm user ticket-id]
  (.deleteTicket (ticket-admin-service cm user) ticket-id))

(defn publicize-ticket
  "Allows the ticket to be viewed by the public group."
  [cm ticket-id]
  (doto (ticket-admin-service cm (:username cm))
    (.addTicketGroupRestriction ticket-id "public")))

(defn ticket?
  "Checks to see if ticket-id is already being used as a ticket
   identifier."
  [cm user ticket-id]
  (.isTicketInUse (ticket-admin-service cm user) ticket-id))

(defn ^Boolean public-ticket?
  "Checks to see if the provided ticket ID is publicly accessible."
  [cm user ticket-id]

  (let [tas    (ticket-admin-service cm user)
        groups (.listAllGroupRestrictionsForSpecifiedTicket tas ticket-id 0)]
    (if (contains? (set groups) "public")
      true
      false)))

(defn ^Ticket ticket-by-id
  "Looks up the ticket by the provided ticket-id string and
   returns an instance of Ticket."
  [cm user ticket-id]
  (.getTicketForSpecifiedTicketString
    (ticket-admin-service cm user)
    ticket-id))

(defn ticket-obj->map
  [^Ticket ticket]
  {:ticket-id        (.getTicketString ticket)
   :path             (.getIrodsAbsolutePath ticket)
   :byte-write-limit (str (.getWriteByteLimit ticket))
   :byte-write-count (str (.getWriteByteCount ticket))
   :uses-limit       (str (.getUsesLimit ticket))
   :uses-count       (str (.getUsesCount ticket))
   :file-write-limit (str (.getWriteFileLimit ticket))
   :file-write-count (str (.getWriteFileCount ticket))
   :expiration       (or (.getExpireTime ticket) "")})

(defn ticket-map
  [cm user ticket-id]
  (ticket-obj->map (ticket-by-id cm user ticket-id)))

(defn ticket-ids-for-path
  [cm user path]
  (let [tas (ticket-admin-service cm user)]
    (case (object-type cm path)
      :dir  (mapv ticket-obj->map (.listAllTicketsForGivenCollection tas path 0))
      :file (mapv ticket-obj->map (.listAllTicketsForGivenDataObject tas path 0)))))

(defn ticket-expired?
  [^Ticket ticket-obj]
  (if (.getExpireTime ticket-obj)
    (.. (java.util.Date.) (after (.getExpireTime ticket-obj)))
    false))

(defn ticket-used-up?
  [^Ticket ticket-obj]
  (> (.getUsesCount ticket-obj) (.getUsesLimit ticket-obj)))

(defn ticket-input-stream
  [cm user ticket-id]
  (input-stream cm (.getIrodsAbsolutePath (ticket-by-id cm user ticket-id))))

(defn ticket-proxy-input-stream
  [cm user ticket-id]
  (proxy-input-stream cm (ticket-input-stream cm user ticket-id)))

(defn iget
  "Transfers remote-path to local-path using a ticket, using tcl as the TransferStatusCallbackListener"
  ([cm ticket-id remote-path local-path tcl]
    (iget cm ticket-id remote-path local-path tcl tcb))
  ([cm ticket-id remote-path local-path tcl control-block]
    (let [tco (ticket-client-operations cm)
          sourceFile (file cm remote-path)
          localFile (File. local-path)]
      (.getOperationFromIRODSUsingTicket tco ticket-id sourceFile localFile tcl control-block))))

(defn iput
  "Transfers local-path to remote-path using a ticket, using tcl as the TransferStatusCallbackListener.
   tcl can also be set to nil."
  ([cm ticket-id local-path remote-path tcl]
    (iput cm ticket-id local-path remote-path tcl tcb))
  ([cm ticket-id local-path remote-path tcl control-block]
    (try
      (let [tco (ticket-client-operations cm)
            targetFile (file cm remote-path)
            localFile (File. local-path)]
        (.putFileToIRODSUsingTicket tco ticket-id localFile targetFile tcl control-block))
      (catch CatNoAccessException _
        (throw+ {:error_code ERR_NOT_WRITEABLE :path remote-path})))))
