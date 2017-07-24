(ns clj-jargon.item-info
  (:use [clj-jargon.validations])
  (:require [clojure-commons.file-utils :as ft])
  (:import [org.irods.jargon.core.pub.domain ObjStat$SpecColType
                                             Collection
                                             DataObject
                                             Quota]
           [org.irods.jargon.core.query CollectionAndDataObjectListingEntry$ObjectType]
           [org.irods.jargon.core.pub IRODSFileSystemAO
                                      DataObjectAO
                                      CollectionAO
                                      QuotaAO]
           [org.irods.jargon.core.pub.io IRODSFile
                                         IRODSFileFactory]))

(def collection-type CollectionAndDataObjectListingEntry$ObjectType/COLLECTION)
(def dataobject-type CollectionAndDataObjectListingEntry$ObjectType/DATA_OBJECT)


(defn ^String trash-base-dir
  "Returns the base trash folder for a specified user.

   Parameters:
     zone - he name of the authenication zone
     user - the username of the user's trash folder to look up.

   Returns:
     It returns the absolute path to the trash folder."
  [^String zone ^String user]
  (ft/path-join "/" zone "trash" "home" user))


(defn ^IRODSFile file
  "Returns an instance of IRODSFile representing 'path'. Note that path
    can point to either a file or a directory.

    Parameters:
      path - String containing a path.

    Returns: An instance of IRODSFile representing 'path'."
  [{^IRODSFileFactory file-factory :fileFactory} ^String path]
  (validate-path-lengths path)
  (.instanceIRODSFile file-factory path))

(defn ^Boolean exists?
  "Returns true if 'path' exists in iRODS and false otherwise.

    Parameters:
      path - String containing a path.

    Returns: true if the path exists in iRODS and false otherwise."
  [cm ^String path]
  (validate-path-lengths path)
  (.exists (file cm path)))

(defn ^Boolean paths-exist?
  "Returns true if the paths exist in iRODS.

    Parameters:
      paths - A sequence of strings containing paths.

    Returns: Boolean"
  [cm paths]
  (doseq [p paths] (validate-path-lengths p))
  (zero? (count (filter #(not (exists? cm %)) paths))))

(defn object-type
  [{^IRODSFileSystemAO cm-ao :fileSystemAO} ^String path]
  (condp = (.getObjectType (.getObjStat cm-ao path))
    collection-type :dir
    dataobject-type :file
    :none))

(defn- ^Boolean jargon-type-check
  [cm check-type ^String path]
  (= check-type (object-type cm path)))

(defn ^Boolean is-file?
  "Returns true if the path is a file in iRODS, false otherwise."
  [cm ^String path]
  (validate-path-lengths path)
  (jargon-type-check cm :file path))

(defn ^Boolean is-dir?
  "Returns true if the path is a directory in iRODS, false otherwise."
  [cm ^String path]
  (validate-path-lengths path)
  (jargon-type-check cm :dir path))

(defn ^Boolean is-linked-dir?
  "Indicates whether or not a directory (collection) is actually a link to a
   directory (linked collection).

   Parameters:
     cm - the context map
     path - the absolute path to the directory to check.

   Returns:
     It returns true if the path points to a linked directory, otherwise it
     returns false."
  [{^IRODSFileFactory file-factory :fileFactory} ^String path]
  (validate-path-lengths path)
  (= ObjStat$SpecColType/LINKED_COLL
     (.. file-factory
       (instanceIRODSFile (ft/rm-last-slash path))
       initializeObjStatForFile
       getSpecColType)))

(defn ^DataObject data-object
  "Returns an instance of DataObject representing 'path'."
  [{^DataObjectAO data-ao :dataObjectAO} ^String path]
  (validate-path-lengths path)
  (.findByAbsolutePath data-ao path))

(defn ^Collection collection
  "Returns an instance of Collection (the Jargon version) representing
    a directory in iRODS."
  [{^CollectionAO collection-ao :collectionAO} ^String path]
  (validate-path-lengths path)
  (.findByAbsolutePath collection-ao (ft/rm-last-slash path)))

(defn lastmod-date
  "Returns the date that the file/directory was last modified."
  [{^IRODSFileSystemAO cm-ao :fileSystemAO} ^String path]
  (validate-path-lengths path)
  (str (long (.getTime (.getModifiedAt (.getObjStat cm-ao path))))))

(defn created-date
  "Returns the date that the file/directory was created."
  [{^IRODSFileSystemAO cm-ao :fileSystemAO} ^String path]
  (validate-path-lengths path)
  (str (long (.getTime (.getCreatedAt (.getObjStat cm-ao path))))))

(defn file-size
  "Returns the size of the file in bytes."
  [{^IRODSFileSystemAO cm-ao :fileSystemAO} ^String path]
  (validate-path-lengths path)
  (.getObjSize (.getObjStat cm-ao path)))

(defn stat
  "Returns status information for a path."
  [{^IRODSFileSystemAO cm-ao :fileSystemAO} ^String path]
  (validate-path-lengths path)
  (let [objstat (.getObjStat cm-ao path)]
    (condp = (.getObjectType objstat)
      collection-type
      {:id            path
       :path          path
       :type          :dir
       :date-created  (long (.getTime (.getCreatedAt objstat)))
       :date-modified (long (.getTime (.getModifiedAt objstat)))}
      dataobject-type
      {:id            path
       :path          path
       :type          :file
       :file-size     (.getObjSize objstat)
       :md5           (.getChecksum objstat)
       :date-created  (long (.getTime (.getCreatedAt objstat)))
       :date-modified (long (.getTime (.getModifiedAt objstat)))
       })))

(defn quota-map
  [^Quota quota-entry]
  (hash-map
    :resource (.getResourceName quota-entry)
    :zone     (.getZoneName quota-entry)
    :user     (.getUserName quota-entry)
    :updated  (str (.getTime (.getUpdatedAt quota-entry)))
    :limit    (str (.getQuotaLimit quota-entry))
    :over     (str (.getQuotaOver quota-entry))))

(defn quota
  [{^QuotaAO quota-ao :quotaAO} ^String user]
  (mapv quota-map (.listQuotaForAUser quota-ao user)))
