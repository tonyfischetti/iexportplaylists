
(asdf:defsystem :iexportplaylists
  :description "Application to help mass-export iTunes playlists"
  :author "Tony Fischetti"
  :license "MIT (Expat)"
  :depends-on (:cl-ppcre :cxml :xpath :drakma)
  :components ((:file "package")
               (:file "conveniences" :depends-on ("package"))
               (:file "iexportplaylists" :depends-on ("package" "conveniences"))))

