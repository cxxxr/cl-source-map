(defsystem "cl-source-map"
  :serial t
  :components ((:file "util")
               (:file "json")
               (:file "base64-vlq")
               (:file "mapping")
               (:file "mapping-list")
               (:file "source-map-generator")))
