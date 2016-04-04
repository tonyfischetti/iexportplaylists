
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                       ;;
;;   iexportplaylists.lisp                               ;;
;;                                                       ;;
;;                Author: Tony Fischetti                 ;;
;;                        tony.fischetti@gmail.com       ;;
;;                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;  If you live and die by your iTunes playlists        ;
;  like I do, fresh installing OS X can be a           ;
;  herculean effort because it, heretofore, required   ;
;  manual export and re-import of all playlists. This  ;
;  program provides the ability to programmatically    ;
;  back up all playlists (in re-importable XML) and    ;
;  export an informal human-readable representation.   ;


; On SBCL, I have to run this with --dynamic-space-size <HIGH NUMBER>

(in-package :iexportplaylists)



; these are the names of "playlists" that aren't music, or that we
; don't want to export
(defparameter *ignored-playlists* '("####!####" "Music" "Music Videos" "Rentals"
                                    "Movies" "Home Videos" "TV Shows" "Podcasts"
                                    "iTunes U" "Audiobooks" "Books" "PDFs"
                                    "Apps" "Purchased" "Genius"))


;---------------------------------------------------------;

(defun get-dom (filename)
  "----------------------------------------------------------
  Takes a filename to the iTunes-Music-Library.xml,
  and parses the xml (uses the DTD). Needs drakma and cxml
  ----------------------------------------------------------"
  (handler-case
    (progn
      (flet ((resolver (pubid sysid)
                       (declare (ignore pubid))
                       (when (eq (puri:uri-scheme sysid) :http)
                         (drakma:http-request sysid :want-stream t))))
        (cxml:parse-file filename (cxml-dom:make-dom-builder)
                         :entity-resolver #'resolver)))
    (error (e) (error "Unrecoverable error parsing library: ~A" e))))
                      

(defun get-child-elements (doc-elem)
  "----------------------------------------------------------
  Helper function to get a list of child elements of an
  cxml element
  ----------------------------------------------------------"
  (remove-if-not (lambda (x) (eq (type-of x) 'RUNE-DOM::ELEMENT))
                 (coerce (dom:child-nodes doc-elem) 'list)))


(defun get-value-of-node (node)
  "----------------------------------------------------------
  Helper function to return the content of a single-content node.
  ----------------------------------------------------------"
  (dom:node-value (elt (dom:child-nodes node) 0)))


(defun get-key-value-hash (node &key (raw nil))
  "----------------------------------------------------------
  Takes an cxml node and returns a hash table of the
  '<key><SOMETHING>---</SOMETHING>' key-value pairs in the node.
  If keyword arg :RAW is T, the values are raw cxml. If NIL
  (default) 'string' and 'integer' nodes are parsed and the
  ~contents~ are the values.
  ----------------------------------------------------------"
  (labels ((get-true-value (raw-value)
    (let ((vtname (dom:tag-name raw-value)))
      (cond ((string= "string"  vtname)    (get-value-of-node raw-value))
            ((string= "integer" vtname)    (get-value-of-node raw-value))
            (t                             raw-value)))))
    (let* ((children (get-child-elements node))
           (the-length (length children))
           (ret-hash (make-hash-table :test #'equal
                                      :size (/ the-length 2))))
      (do ((key-el 0 (+ 2 key-el)))
        ((>= key-el the-length) ret-hash)
        (let ((key     (dom:node-value (elt (dom:child-nodes
                                              (elt children key-el)) 0)))
              (vraw    (elt children (+ 1 key-el))))
          (cond
            (raw        (add-to-hash ret-hash key vraw))
            (t          (add-to-hash ret-hash key (get-true-value vraw)))))))))




(defun get-value-at-key (node the-key)
  "----------------------------------------------------------
  Takes a cxml node and a key (a string). It turns the node
  into a key-value hash-table and returns the value at that key.
  ----------------------------------------------------------"
  (gethash the-key (get-key-value-hash node)))


(defun get-playlist-track-ids (playlist)
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist
  and returns a list of the playlist's track IDs. If it's an
  improper playlist (no 'Playlist Items' attribute or '<integer>'
  track-id values, then the list is NIL. This must be checked
  for downstream. (needs xpath pacakge)
  ----------------------------------------------------------"
  (handler-case 
    (progn
      (let* ((items      (get-value-at-key playlist "Playlist Items"))
             (trax       (xpath:evaluate ".//integer" items))
             (ret-list   '()))
        (xpath:do-node-set (track trax)
          (push (get-value-of-node track) ret-list))
        (nreverse ret-list)))
    (error () nil)))


(defun export-playlists-simple-string (PLAYLISTS IGNORED-PLAYLISTS TRACKS-HASH
                                        &key (quiet? nil))
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist
    - an 'array' dom element (representing all playlists)
    - a list of names representing playlists to skip
    - a hash-table where track IDs are keys and the track dict are
    the values
  And returns a string that is a simple human-readable export of
  of the playlists and their contents.
  ----------------------------------------------------------"
  (let ((s (make-string-output-stream)))
    (for-each (count playlist (get-child-elements PLAYLISTS)
               :progress? (not quiet?))
      (let* ((playlist-name (get-value-at-key playlist "Name"))
             (in? (find playlist-name IGNORED-PLAYLISTS :test #'equalp)))
        (unless in?
          (unless quiet? (format t "On playlist: ~A~%" playlist-name))
          (let ((trax (get-playlist-track-ids playlist)))
            (unless (null trax)
              (format s "~A:~%" playlist-name)
              (for-each (num track (get-playlist-track-ids playlist))
                (let* ((track-hash (gethash track TRACKS-HASH))
                       (name       (get-value-at-key track-hash "Name"))
                       (artist     (get-value-at-key track-hash "Artist"))
                       (album      (get-value-at-key track-hash "Album")))
                  (format s "  ~A: ~A~42t(~A) [~A]~%" (+ num 1)
                          name artist album)))
              (format s "~%"))))))
    (get-output-stream-string s)))


;;;;;;;;;;;;;;;;;;;;;;
;;; for xml export ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-track-xml-string (song-key TRACKS-HASH)
  "----------------------------------------------------------
  Takes a song-key (string of numbers) and the hash-table of tracks
  and returns a string that is an xml representation of a track in
  an exported playlist
  ----------------------------------------------------------"
  (let* ((track-dict (gethash song-key TRACKS-HASH)))
    (let ((s (make-string-output-stream)))
      (format s "~A~A<key>~A</key>~%~A~A<dict>"
              #\tab #\tab song-key #\tab #\tab)
      (dom:map-document (cxml:make-character-stream-sink s
                          :omit-xml-declaration-p t) track-dict)
      (format s "</dict>~%")
      (get-output-stream-string s))))


(defun make-tracks-xml-string (a-playlist TRACKS-HASH)
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist (and the
  hash-table of tracks for downstream) and returns a string
  that is an XML representation of the '<key>Tracks</key>'
  part of a exported playlist XML
  ----------------------------------------------------------"
  (let* ((s (make-string-output-stream))
         (list-of-song-keys (get-playlist-track-ids a-playlist)))
    (format s "~A<key>Tracks</key>~%~A<dict>~%" #\tab #\tab)
    (for-each (index a-key list-of-song-keys)
      (format s (make-track-xml-string a-key TRACKS-HASH)))
    (format s "~A</dict>~%" #\tab)
    (get-output-stream-string s)))


(defun make-playlist-key-xml-string (a-playlist)
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist 
  and returns a string that is an XML representation of the
  '<key>Playlists</key>' part of a exported playlist XML
  ----------------------------------------------------------"
  (let* ((s (make-string-output-stream)))
    (format s "~A<key>Playlists</key>~%~A<array>~%~A~A<dict>"
            #\tab #\tab #\tab #\tab)
    (dom:map-document (cxml:make-character-stream-sink s
                        :omit-xml-declaration-p t) a-playlist)
    (format s "</dict>~%~A</array>~%" #\tab)
    (get-output-stream-string s)))


(defun make-playlist-xml-string (a-playlist TRACKS-HASH)
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist (and the
  hash-table of tracks for downstream) and returns a string
  that combines the output of `make-tracks-xml-string` and
  `make-playlist-key-xml-string`.  This is almost a complete
  string representation of an exported playlist's XML, it just
  needs some headers and footers
  ----------------------------------------------------------"
  (let* ((s (make-string-output-stream)))
    (format s (make-tracks-xml-string a-playlist TRACKS-HASH))
    (format s (make-playlist-key-xml-string a-playlist))
    (get-output-stream-string s)))


(defun export-playlist-string (a-playlist TRACKS-HASH)
  "----------------------------------------------------------
  Takes a '<dict>' DOM element representing a playlist (and the
  hash-table of tracks for downstream) and returns a complete 
  string representation of an exported playlist's XML.
  ----------------------------------------------------------"
  (let* ((s (make-string-output-stream)))
    (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format s "<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0")
    (format s "//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">~%")
    (format s "<plist version=\"1.0\">~%")
    (format s "<dict>~%")
    (format s (make-playlist-xml-string a-playlist TRACKS-HASH))
    (format s "</dict>~%")
    (format s "</plist>")
    (get-output-stream-string s)))


; ------------------------------------------------- ;

(defun guess-library-location ()
  "Tries to guess location of iTunes Music Library XML"
  (namestring (make-pathname
                :defaults (merge-pathnames #p"Music/iTunes/"
                                           (user-homedir-pathname))
                :name "iTunes Music Library" :type "xml")))

(defun escape-filename (fn)
  "Make sure there's no funny business like slashes in playlist names
  that would mess up the export process"
  (cl-ppcre:regex-replace-all "[^\\w !\\.]" fn "_"))

(defun prompt-string-with-cwd (a-string)
  "Helper function to construct a string (for prompting)
  post-fixed with the current directory"
  (let ((cwd (namestring (truename "."))))
    (format nil "~A [current directory: '~A']: " a-string cwd)))

(defun prompt-read (prompt)
  "Stolen from 'Practical Common Lisp' by Peter Seibel"
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun pathname-as-directory (name)
  "Stolen from 'Practical Common Lisp' by Peter Seibel"
  (labels (
    (component-present-p (value)
      (and value (not (eql value :unspecific))))
    (directory-pathname? (p)
      (and (not (component-present-p (pathname-name p)))
           (not (component-present-p (pathname-type p))) p)))
    (let ((pathname (pathname name)))
      (if (not (directory-pathname? name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name      nil
         :type      nil
         :defaults pathname)
        pathname))))

(defun interact-simple-export (PLAYLISTS IGNORED-PLAYLISTS TRACKS-HASH
                               &key (a-file nil))
  "Grabs a file name to export to and exports to it"
  (let ((fn
          (if a-file
            a-file
            (prompt-read (prompt-string-with-cwd
                           "Enter filename to export to")))))
    (let ((overwrite? nil))
      (when (probe-file fn)
        (if (y-or-n-p "File already exists. Overwrite?")
          (setf overwrite? :supersede)
          (return-from interact-simple-export nil)))
      (with-open-file (stream fn :direction :output
                                 :if-exists overwrite?)
        (format stream "~A" (export-playlists-simple-string PLAYLISTS
                                                            IGNORED-PLAYLISTS
                                                            TRACKS-HASH)))
      (format t "Done.~%"))))

(defun interact-xml-export (PLAYLISTS IGNORED-PLAYLISTS TRACKS-HASH
                            &key (a-directory nil))
  "Grabs a directory to dump each individual XML export into
  it with the name of the playlist as the file name"
  (let* ((default-dir (namestring (merge-pathnames "xmls" (truename "."))))
         (the-dir     (if a-directory
                        a-directory 
                        (prompt-read (concatenate 'string
                                        "Enter directory to dump exports "
                                        "to [default: '" default-dir
                                        "']: ")))))
    (if (string= "" the-dir)
      (setf the-dir (pathname-as-directory default-dir))
      (setf the-dir (pathname-as-directory the-dir)))
    (ensure-directories-exist the-dir)
    (for-each (count playlist (get-child-elements PLAYLISTS) :progress? T)
      (let* ((playlist-name (get-value-at-key playlist "Name"))
             (in? (find playlist-name IGNORED-PLAYLISTS :test #'equalp)))
        (unless in?
          (let ((trax (get-playlist-track-ids playlist)))
            (unless (null trax)
              (let ((fn (namestring (make-pathname :defaults the-dir
                                                   :name (escape-filename
                                                           playlist-name)
                                                   :type "xml"))))
                (format t "Going to export playlist: ~A to ~A~%"
                        playlist-name fn)
                (let ((overwrite? nil))
                  (when (probe-file fn)
                    (if (y-or-n-p "File already exists. Overwrite?")
                      (setf overwrite? :supersede)
                      (return-from interact-xml-export)))
                  (with-open-file (stream fn :direction :output
                                             :if-exists overwrite?)
                    (format stream "~A" (export-playlist-string
                                          playlist TRACKS-HASH))))))))))
    (format t "Done.~%")))

(defun get-playlists-and-tracks-hash (&key (ask t))
  (let* ((WHOLE-THING nil)
         (OUTER-DICT  nil)
         (PLAYLISTS   nil)
         (TRACKS-HASH nil)
         (guessed-path (guess-library-location))
         (path (if ask
                 (prompt-read (concatenate 'string
                                "Path to music library XML [default: "
                                guessed-path "]: "))
                 guessed-path)))
    (when (string= "" path)
      (setf path guessed-path))
    (assert (probe-file path) (path) "~A doesn't exist" path)
    (explain (format nil "Attempting to read and parse ~A...."
                     (file-namestring path))
      (setf WHOLE-THING (get-dom path)))
    (format t "Parsed library successfully~%")
    ; add error checking?
    (setf OUTER-DICT (car (get-child-elements
                            (car (get-child-elements WHOLE-THING)))))
    (explain "Extracting playlists..."
      (setf PLAYLISTS (get-value-at-key OUTER-DICT "Playlists")))
    (explain "Extracting track information..."
      (setf TRACKS-HASH (get-key-value-hash
                          (get-value-at-key OUTER-DICT "Tracks"))))
    (list PLAYLISTS TRACKS-HASH)))

(defun main ()
  (let* ((result      (get-playlists-and-tracks-hash))
         (PLAYLISTS   (car result))
         (TRACKS-HASH (cadr result)))
    (block interaction
      (loop
        (format t "~%Please choose one...~%")
        (format t "  1) Export playlists as a human-readable text file~%")
        (format t "  2) Export playlists as importable XML docs in directory~%")
        (format t "  q) Quit~% ")
        (let ((response (prompt-read "> ")))
          (cond
            ((string= response "1")
               (interact-simple-export PLAYLISTS
                                       *ignored-playlists* TRACKS-HASH))
            ((string= response "2")
               (interact-xml-export PLAYLISTS *ignored-playlists* TRACKS-HASH))
            ((or (equalp response "q") (equalp response "quit"))
               (progn
                 (format t "Bye~%")
                 (return-from interaction)))
            (t
              (format t "Unrecognized input (choose '1' '2' or 'q')~%"))))))))


; (main)
