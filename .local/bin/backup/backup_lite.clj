#!/usr/bin/env bb
(ns backup
  (:require
    [babashka.fs :as fs]
    [babashka.process :as p]
    [clojure.edn :as edn]
    [clojure.string :as str]))

(def ^:private script-version "0.2.0")

(defn- script-dir []
  (-> (or (System/getProperty "babashka.file") *file*)
      fs/path fs/absolutize fs/parent))

(defn- load-config []
  (let [path (fs/path (script-dir) "config.edn")]
    (when-not (fs/exists? path)
      (println (str "ERROR: config.edn not found at " path))
      (System/exit 1))
    (edn/read-string (slurp (str path)))))

(def ^:private ts-formatter
  (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

(def ^:private file-formatter
  (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))

(defn- now [] (.format ts-formatter (java.time.LocalDateTime/now)))
(defn- file-ts [] (.format file-formatter (java.time.LocalDateTime/now)))

(defn- normalize-path [s]
  (str (fs/normalize (fs/absolutize (fs/expand-home s)))))

(defn- hostname []
  (or (some-> "/etc/hostname" fs/path
              (#(when (fs/exists? %) (str/trim (slurp (str %))))))
      (let [r @(p/process ["hostname"] {:out :string})]
        (when (zero? (:exit r)) (str/trim (:out r))))
      "unknown-host"))

(defn- join-path [& parts]
  (->> parts
       (map #(some-> % str (str/replace #"^/+" "") (str/replace #"/+$" "")))
       (remove str/blank?)
       (str/join "/")))

(defn- resolve-path-from-script-dir [p]
  (when-let [p (some-> p fs/expand-home fs/path)]
    (let [p (if (fs/absolute? p) p (fs/path (script-dir) p))]
      (str (fs/normalize p)))))

(defn- default-exclude-path []
  (let [p (fs/path (script-dir) "exclude.txt")]
    (when (fs/exists? p) (str p))))

(defn- normalize-pattern [s]
  (when-let [s (some-> s str str/trim not-empty)]
    (cond
      (str/starts-with? s "#") nil
      (or (str/starts-with? s "/") (str/starts-with? s "~")) (normalize-path s)
      :else s)))

(defn- read-patterns-from-file [path]
  (when-let [path (some-> path resolve-path-from-script-dir)]
    (when (fs/exists? (fs/path path))
      (->> (slurp path)
           str/split-lines
           (map normalize-pattern)
           (remove nil?)
           vec))))

(defn- collect-patterns [{:keys [abs-file patterns]}]
  (let [abs-file (or abs-file (default-exclude-path))]
    (->> (concat (read-patterns-from-file abs-file)
                 (->> (or patterns [])
                      (map normalize-pattern)
                      (remove nil?)))
       distinct
       vec)))

(defn- relative-excludes
  "Для каждого абсолютного паттерна обрезает префикс источника.
   Паттерны которые не начинаются с пути источника — пропускаются.
   Например: источник /home/user/Documents
             паттерн  /home/user/Documents/Projects/**
             результат Projects/**"
  [abs-source all-patterns]
  (let [root (normalize-path abs-source)
        prefix (str root "/")]
    (->> all-patterns
         (keep (fn [pat]
                 (cond
                   (str/blank? pat) nil
                   ;; относительные паттерны (например ".git/") применяем ко всем источникам как есть
                   (not (str/starts-with? pat "/")) pat
                   (str/starts-with? pat prefix) (let [rel (subs pat (count prefix))]
                                                   (if (str/blank? rel) "**" rel))
                   (= pat root) "**"
                   :else nil)))
         vec)))

(defn- make-log! [writer]
  (fn [level msg]
    (let [line (str (now) " [" (str/upper-case (name level)) "] " msg)]
      (.write ^java.io.Writer writer (str line "\n"))
      (.flush ^java.io.Writer writer)
      (println line))))

(defn- run-cmd!
  "Запускает команду, пробрасывает stdout/stderr напрямую в терминал.
   Возвращает exit-код."
  [log! cmd]
  (log! :info (str "RUN " (pr-str (vec cmd))))
  (let [exit (:exit @(p/process (vec cmd) {:out :inherit :err :inherit}))]
    (log! (if (zero? exit) :info :warn) (str "EXIT " exit))
    exit))

;; ---------------------------------------------------------------------------
;; rsync
;; ---------------------------------------------------------------------------

(defn- rsync!
  [log! {:keys [mount prefix host flags exclude-from allow-exit-codes dry-run?]} patterns sources]
  (doseq [src sources]
    (let [dest (str (fs/path mount prefix host
                              ;; убираем ведущий / чтобы не получить абсолютный путь внутри mount
                              (subs (normalize-path src) 1)))
          rel-excludes (relative-excludes src patterns)
          cmd (cond-> ["rsync" "-a" "--mkpath"]
                dry-run?                  (conj "--dry-run")
                exclude-from              (conj "--exclude-from" exclude-from)
                (seq rel-excludes)        (into (mapcat #(vector "--exclude" %) rel-excludes))
                (seq flags)               (into flags)
                (fs/directory? src)       (conj (str src "/") (str dest "/"))
                (not (fs/directory? src)) (conj src dest))
          exit (run-cmd! log! cmd)]
      (when-not (or (zero? exit) (contains? (set allow-exit-codes) exit))
        (println (str "ERROR: rsync failed with exit " exit " for " src))
        (System/exit exit)))))

;; ---------------------------------------------------------------------------
;; rclone
;; ---------------------------------------------------------------------------

(defn- rclone!
  [log! {:keys [remote prefix host flags exclude-from filter-from allow-exit-codes dry-run?]} patterns sources]
  (doseq [src sources]
    (let [;; rclone принимает remote:prefix/host/rel/path
          rel (subs (normalize-path src) 1)
          dest (str remote (join-path prefix host rel))
          rel-excludes (relative-excludes src patterns)
          cmd (cond-> ["rclone" "copy" src dest]
                dry-run?               (conj "--dry-run")
                exclude-from           (conj "--exclude-from" exclude-from)
                filter-from            (conj "--filter-from" filter-from)
                (seq rel-excludes)     (into (mapcat #(vector "--exclude" %) rel-excludes))
                (seq flags)            (into flags))
          exit (run-cmd! log! cmd)]
      (when-not (or (zero? exit) (contains? (set allow-exit-codes) exit))
        (println (str "ERROR: rclone failed with exit " exit " for " src))
        (System/exit exit)))))

;; ---------------------------------------------------------------------------
;; Профили и точка входа
;; ---------------------------------------------------------------------------

(defn- resolve-sources [sources missing-source log!]
  (->> sources
       (map normalize-path)
       (filter (fn [s]
                 (if (fs/exists? s)
                   true
                   (case missing-source
                     :skip (do (log! :warn (str "Source not found, skipping: " s)) false)
                     (do (println (str "ERROR: source not found: " s)) (System/exit 2))))))))

(defn- parse-args [args]
  (let [args (vec args)
        flags (set (filter #(str/starts-with? % "-") args))
        positional (remove #(str/starts-with? % "-") args)]
    {:dry-run? (boolean (or (flags "-n") (flags "--dry-run")))
     :profile  (some #(when (not (str/starts-with? % "-")) (keyword %)) positional)}))

(defn- run! [args]
  (let [config   (load-config)
        {:keys [dry-run? profile]} (parse-args args)
        profile  (or profile (:default-profile config))
        _        (when-not profile
                   (println "ERROR: no profile selected. Pass profile name as argument or set :default-profile in config.edn")
                   (System/exit 2))
        pcfg     (get-in config [:profiles profile])
        _        (when-not pcfg
                   (println (str "ERROR: unknown profile " profile
                                 " (known: " (str/join ", " (map name (keys (:profiles config)))) ")"))
                   (System/exit 2))
        host     (hostname)
        log-dir  (or (get-in config [:log :dir]) "./logs")
        log-dir  (if (fs/absolute? (fs/path log-dir)) log-dir (str (fs/path (script-dir) log-dir)))
        log-path (str (fs/path log-dir (str "backup-" (file-ts) ".log")))
        _        (fs/create-dirs (fs/parent log-path))
        patterns (collect-patterns (:exclude config))
        sources  (vec (:sources pcfg))
        missing  (or (:missing-source pcfg) :skip)
        targets  (set (:targets pcfg))]
    (with-open [w (java.io.FileWriter. log-path true)]
      (let [log! (make-log! w)]
        (log! :info (str "bb-backup v" script-version " profile=" (name profile)
                         " host=" host " dry-run=" dry-run?))
        (let [sources (resolve-sources sources missing log!)]
          (when (empty? sources)
            (println "ERROR: no existing source paths")
            (System/exit 2))
          (when (targets :disk)
            (rsync! log!
                    (merge (:disk pcfg)
                           {:host host :dry-run? dry-run?
                            :allow-exit-codes (get-in config [:allow-exit-codes :rsync])})
                    patterns sources))
          (when (targets :cloud)
            (rclone! log!
                     (merge (:cloud pcfg)
                            {:host host :dry-run? dry-run?
                             :allow-exit-codes (get-in config [:allow-exit-codes :rclone])})
                     patterns sources)))
        (log! :info "DONE")))))

(run! *command-line-args*)
