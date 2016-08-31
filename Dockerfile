FROM clojure
COPY . /usr/src/clj-jargon
COPY ./docker/profiles.clj /root/.lein/profiles.clj
WORKDIR /usr/src/clj-jargon
RUN lein deps
CMD ["lein", "test"]
