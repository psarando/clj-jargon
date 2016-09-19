FROM clojure
COPY ./docker/profiles.clj /root/.lein/profiles.clj
WORKDIR /usr/src/clj-jargon

COPY project.clj /usr/src/clj-jargon/
RUN lein deps

COPY . /usr/src/clj-jargon
CMD ["lein", "test"]
