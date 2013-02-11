;;; javadoc-lookup.el --- Javadoc Emacs integration with Maven

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/javadoc-lookup
;; Version: 1.0

;;; Commentary:

;; This package provides a quick way to look up any Javadoc
;; documentation from Emacs, using your browser to display the
;; information. Since the mechanism is already there, java-import.el
;; provides the completing function `add-java-import' for quickly
;; adding an import to a source file.

;; This mode stores database and index information in
;; `javadoc-lookup-cache-dir'.

;; Indexing:

;; Give `javadoc-lookup' your root Java documentation directories. It
;; will scan and index those directories, exposing them to
;; `javadoc-lookup'. Multiple directories can be provided at once, for
;; example,

;;   (javadoc-add-roots "/usr/share/doc/openjdk-6-jdk/api"
;;                      "~/src/project/doc")

;; If you haven't loaded the core Java Javadoc, it will load a
;; pre-made database for you, which indexes the official website.

;; More conveniently, you can list Maven artifacts to index,

;;   (javadoc-add-artifacts [org.lwjgl.lwjgl lwjgl "2.8.2"]
;;                          [com.nullprogram native-guide "0.2"]
;;                          [org.apache.commons commons-math3 "3.0"])

;; Browser configuration:

;; To view documentation, the browser is launched with `browse-url'.
;; This may require setting `browse-url-browser-function' in order to
;; select the proper browser. For example,

;;   (setq browse-url-browser-function 'browse-url-firefox)

;;; Code:

(require 'cl)
(require 'ido)

(defgroup javadoc-lookup ()
  "Lookup Java library documentation from Emacs."
  :group 'java)

;; Customization variables

(defcustom javadoc-lookup-cache-dir (locate-user-emacs-file "javadoc-cache")
  "Filesystem location to store index and cache database.")

(defcustom javadoc-lookup-completing-read-function #'ido-completing-read
  "Function used when performing a minibuffer read.")

(defcustom javadoc-lookup-use-method-lookup nil
  "Non nil to use lookup by method.")

;; Internal variables

(defvar jdl/data-root (file-name-directory load-file-name)
  "The location of data for javadoc-loookup.")

(defvar jdl/index (make-hash-table :test 'equal)
  "Full index of for documentation lookups.")

(defvar jdl/cache-version ".v4"
  "Cache version, so it won't load old caches.")

(defvar jdl/loaded ()
  "List of already-loaded documentation directories.")

(defvar jdl/method-table-lst nil
  "List of hash tables.
Each hash table is as follows:
method => '((class1 . url1) (class2 . url2))")

;; Indexing Functions

(defun jdl/dir-truename (dir)
  "Return the truename for DIR, which always has a trailing slash."
  (expand-file-name (concat dir "/")))

(defun jdl/clear ()
  "Clear all in-memory javadoc-lookup cache and indexes."
  (setq jdl/loaded nil)
  (setq jdl/index (make-hash-table :test 'equal))
  (setq jdl/method-table-lst nil))

(defun jdl/loaded-p (dir)
  "Return t if DIR has already been loaded."
  (member dir jdl/loaded))

(defun jdl/cache-name (dir)
  "Get the cache file name for DIR."
  (concat (replace-regexp-in-string "[/:]" "+" dir) jdl/cache-version))

(defun jdl/load-cache (cache-file)
  "Load a cache from disk."
  (with-current-buffer (find-file-noselect cache-file)
    (goto-char (point-min))
    (jdl/add-hash (read (current-buffer)))
    (kill-buffer)))

(defun jdl/save-cache (cache-file hash)
  "Save a cache to the disk."
  (unless (file-exists-p javadoc-lookup-cache-dir)
    (make-directory javadoc-lookup-cache-dir t))
  (with-temp-file cache-file
    (let ((print-circle t))
      (prin1 hash (current-buffer)))))

(defun jdl/add (dir)
  "Index DIR, using the cache if available."
  (let ((cache-file (expand-file-name (jdl/cache-name dir)
                                      javadoc-lookup-cache-dir)))
    (if (file-exists-p cache-file)
        (jdl/load-cache cache-file)
      (let ((hash (make-hash-table :test 'equal)))
        (jdl/index dir hash)
        (jdl/save-cache cache-file hash)
        (jdl/add-hash hash))))
  (add-to-list 'jdl/loaded dir))

(defun jdl/add-hash (hash)
  "Combine HASH into the main index hash."
  (maphash (lambda (key val) (puthash key val jdl/index)) hash))

(defun* jdl/index (dir hash &optional (root (list dir "file://")))
  "Index the documentation in DIR into HASH, based on ROOT."
  (let* ((list (directory-files dir t "^[^.]"))
         (files (remove-if 'file-directory-p list))
         (dirs (remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (jdl/add-file file hash root))
    (dolist (dir dirs)
      (when (not (string-equal "class-use" (file-name-nondirectory dir)))
        (jdl/index dir hash root)))))

(defun jdl/add-file (fullfile hash root)
  "Add a file to the index if it looks like a class."
  (let* ((file (file-name-nondirectory fullfile))
         (ext (file-name-extension fullfile))
         (class (file-name-sans-extension file))
         (rel (substring fullfile (length (first root))))
         (fullclass (substitute ?. ?/ (file-name-sans-extension rel)))
         (case-fold-search nil))
    (when (and (string-equal ext "html")
               (string-match "^[A-Z].+" class))
      (puthash fullclass (cons rel root) hash))))

(defun javadoc-add-roots (&rest directories)
  "Index and load all documentation under DIRECTORIES."
  (loop for directory in directories
        ;; Directory names have to be without trailing slash.
        ;; Add the trailing slash to directory names
        for truename = (jdl/dir-truename directory)
        for table = nil
        unless (jdl/loaded-p truename)
        do (progn (jdl/add truename)
                  (when javadoc-lookup-use-method-lookup
                    (setq table (jdl/build-method-table truename))
                    (and table
                         (push table jdl/method-table-lst))))))

(defun jdl/build-method-table (docroot-dir)
  "Return a hash table containing methods indices."
  (jdl/parse-index-all-html (concat
                             (directory-file-name
                              (expand-file-name docroot-dir))
                             "/index-all.html")))

(defun jdl/web (&rest urls)
  "Load pre-cached web indexes for URLS."
  (dolist (url (remove-if 'jdl/loaded-p urls))
    (let* ((rel-cache-file (concat "webcache/" (jdl/cache-name url)))
           (cache-file (expand-file-name rel-cache-file jdl/data-root)))
      (if (file-exists-p cache-file)
          (jdl/load-cache cache-file)
        (error "No cache for %s" url)))))

;; Lookup functions

(defun jdl/core-indexed-p ()
  "Return true if the JRE Javadoc has been indexed. The class
java.net.URL is used for this test, since it's simple and should
always be there."
  (gethash "java.net.URL" jdl/index))

(defun jdl/get-class-list ()
  (loop for class being the hash-keys of jdl/index
        collect class into classes
        finally (return (sort* classes #'< :key #'length))))

(defun jdl/completing-read ()
  "Query the user for a class name."
  (unless (jdl/core-indexed-p)
    (ignore-errors ; Provide *something* useful, if needed
      (jdl/web "http://docs.oracle.com/javase/7/docs/api/")))
  (funcall javadoc-lookup-completing-read-function "Class: "
           (jdl/get-class-list)))

(defun jdl/parse-entry (entry)
  (let* ((method (jdl/extract-method-name entry))
         (class (and method
                     (jdl/extract-class-name entry)))
         (url (and method
                   (jdl/extract-url entry))))
    (and method class url
         (not (string= method "nil"))
         (not (string= class "nil"))
         (not (string= url "nil"))
         (list method class url))))

(defun jdl/replce-mark-in-string (string)
  (replace-regexp-in-string
   "&lt;" "<" (replace-regexp-in-string
               "&gt;" ">" string)))

(defun jdl/extract-method-name (entry)
  "Extract method or constructor name."
  (let ((name (jdl/extract-string-by-regexp entry "<b>" "</b>")))
    (when (and name (string-match "(" name))
      (jdl/replce-mark-in-string name))))

(defun jdl/extract-string-by-regexp (string beg-regexp end-regexp)
  "Return substring of string between BEG-REGEXP and END-REGEXP."
  (let* ((beg (and (string-match beg-regexp string)
                   (match-end 0)))
         (end (and beg (string-match end-regexp string beg))))
    (and beg
         end
         (substring-no-properties string beg end))))

(defun jdl/extract-class-name (entry)
  (let ((class-name
         (jdl/extract-string-by-regexp entry
                                       "\\(in\\|for\\) class "
                                       "</a>")))
    (when class-name
      (jdl/replce-mark-in-string
       (jdl/remove-anchor-tag-in-classname class-name)))))

(defun jdl/remove-anchor-tag-in-classname (classname)
  ;; class-name should not contain '</a>'.
  (replace-regexp-in-string
   "<a.*>" "" classname))

(defun jdl/extract-url (entry)
  (let ((url (jdl/extract-string-by-regexp entry
                                           "<a href=\"\\./"
                                           "\"")))
    (when url
      (jdl/replce-mark-in-string url))))

(defun* jdl/parse-index-all-html (file &optional (scheme "file://"))
  "Return a hash table containing all methods and constructors
listed in FILE. A key is a method name, and a value is a list of
cons cells which is (class . url)."
  ;; FILE should be index.html
  (let* ((filepath (expand-file-name file))
         (root-dir (file-name-directory filepath))
         (beg-pos nil)
         (end-pos nil)
         (ret nil)
         (item nil)
         (table (make-hash-table :test #'equal))
         (reporter nil))
    (when (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (goto-char (point-min))
        (setq reporter (make-progress-reporter
                        "Parsing..." (point-min) (point-max)))
        (re-search-forward "<DT>" nil t 1)
        (setq beg-pos (match-beginning 0))
        ;; BUG
        ;; the last entry cannot be parsed.
        (while (re-search-forward "<DT>" nil t)
          (setq end-pos (1- (match-beginning 0)))
          (setq item (jdl/parse-entry
                      (buffer-substring-no-properties beg-pos
                                                      end-pos)))
          (when item
            (jdl/insert-method-into-table item scheme root-dir table))
          (setq beg-pos (1+ end-pos))
          (progress-reporter-update reporter (point)))
        (progress-reporter-done reporter))
      table)))

(defun jdl/insert-method-into-table (item scheme root-dir table)
  (let* ((method (nth 0 item))
         (class (nth 1 item))
         (url (nth 2 item)))
    (when (and method class url
               (not (string= method "nil"))
               (not (string= class "nil"))
               (not (string= url "nil")))
      (push (cons class (concat scheme root-dir url))
            (gethash method
                     table)))))

(defun jdl/collect-methods (table-lst)
  (loop for table in table-lst
        append (loop for method being the hash-keys of table
                     collect method)))

(defun jdl/collect-classes (method table-lst)
  (loop for table in table-lst
        append (mapcar #'car (gethash method table))))

;;;###autoload
(defun javadoc-lookup (name)
  "Lookup based on class name."
  (interactive (list (jdl/completing-read)))
  (let ((file (apply #'concat (reverse (gethash name jdl/index)))))
    (when file (browse-url file))))

(defun javadoc-load-javadoc (dir)
  "Load another javadoc."
  (interactive "DRoot dir of javadoc: ")
  (javadoc-add-roots (directory-file-name
                      (expand-file-name dir))))

(defun javadoc-unload-javadoc (dir)
  "Unload one of the currently loaded javadocs."
  (interactive (list (completing-read
                      "Javadoc to unload: "
                      jdl/loaded
                      nil
                      t)))
  (let ((loaded-lst jdl/loaded))
    (jdl/clear)
    ;; jdl/loaded holds directory names which must end with the
    ;; trailing slash.
    (apply #'javadoc-add-roots
           (delete (concat (directory-file-name dir) "/")
                   loaded-lst))))

(defun javadoc-method-lookup (method &optional class)
  "Lookup based on a method name."
  (interactive (list (funcall javadoc-lookup-completing-read-function
                              "Method: "
                              (jdl/collect-methods jdl/method-table-lst)
                              nil
                              t
                              (thing-at-point 'symbol))))
  (let* ((class (jdl/read-class-from-minibuffer method jdl/method-table-lst))
         (url (jdl/get-url-by-method method class jdl/method-table-lst)))
    (cond
     ((null url)
      (message "javadoc-method-lookup, url is null"))
     ((> (length url) 1)
      (message "javadoc-method-lookup, length of url is %s, url=%s"
               (length url)
               url))
     (t
      (browse-url (car url))))))

(defun jdl/read-class-from-minibuffer (method table-lst)
  "Read class name from minibuffer."
  ;; Should we not prompt if the number of classes is just one?
  (funcall javadoc-lookup-completing-read-function
           (concat method " in ")
           (jdl/collect-classes method table-lst)
           nil
           t))

(defun jdl/get-url-by-method (method class table-lst)
  "Return a list of urls."
  (loop for table in table-lst
        for cell = (assoc class (gethash method table))
        when cell
        collect (cdr cell)))

(provide 'javadoc-lookup)

;;; javadoc-lookup.el ends here
