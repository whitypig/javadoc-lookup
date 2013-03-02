(require 'cl)
(require 'ert)

(ert-deftest test-jdl:jdl/parse-entry ()
  (should
   (equal
    '("AllOf(Iterable<Matcher<? super T>>)"
      "org.hamcrest.core.AllOf"
      "./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)")
    (let ((entry
           "<dt><a href=\"./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)\"><b>AllOf(Iterable&lt;Matcher&lt;? super T&gt;&gt;)</b></a> - 
Constructor for class org.hamcrest.core.<a href=\"./org/hamcrest/core/AllOf.html\" title=\"class in org.hamcrest.core\">AllOf</a>
</dt>"))
      (jdl/parse-entry entry
                       #'jdl/extract-method-name
                       #'jdl/extract-class-name
                       #'jdl/extract-url)))))

(ert-deftest test-jdl/extract-method-name ()
  (let ((entry
         "<dt><a href=\"./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)\"><b>AllOf(Iterable&lt;Matcher&lt;? super T&gt;&gt;)</b></a> - 
Constructor for class org.hamcrest.core.<a href=\"./org/hamcrest/core/AllOf.html\" title=\"class in org.hamcrest.core\">AllOf</a>
</dt>"))
    (should
     (string= "AllOf(Iterable<Matcher<? super T>>)"
              (jdl/extract-method-name entry)))))


(ert-deftest test-jdl/extract-class-name ()
  (let ((entry
         "<dt><a href=\"./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)\"><b>AllOf(Iterable&lt;Matcher&lt;? super T&gt;&gt;)</b></a> - 
Constructor for class org.hamcrest.core.<a href=\"./org/hamcrest/core/AllOf.html\" title=\"class in org.hamcrest.core\">AllOf</a>
</dt>"))
    (should
     (string= "org.hamcrest.core.AllOf"
              (jdl/extract-class-name entry))))
  (let ((entry
         "<dt><a href=\"./org/jmock/syntax/ArgumentConstraintPhrases.html#with(org.hamcrest.Matcher)\"><b>with(Matcher&lt;Boolean&gt;)</b></a> - 
Method in interface org.jmock.syntax.<a href=\"./org/jmock/syntax/ArgumentConstraintPhrases.html\" title=\"interface in org.jmock.syntax\">ArgumentConstraintPhrases</a>
</dt>"))
    (should
     (string= nil
              (jdl/extract-class-name entry)))))

(ert-deftest test-jdl/extract-string-by-regexp ()
  (let ((entry
         "<dt><span class=\"strong\"><a href=\"../java/awt/Component.html#getWidth()\">getWidth()</a></span> - Method in class java.awt.<a href=\"../java/awt/Component.html\" title=\"class in java.awt\">Component</a></dt>"))
    (should
     (string=
      "getWidth()"
      (jdl/extract-string-by-regexp entry
                                    "<a[^>]+>"
                                    "</a>")))))

(ert-deftest test-jdl/extract-url ()
  (let ((entry
         "<dt><a href=\"./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)\"><b>AllOf(Iterable&lt;Matcher&lt;? super T&gt;&gt;)</b></a> - 
Constructor for class org.hamcrest.core.<a href=\"./org/hamcrest/core/AllOf.html\" title=\"class in org.hamcrest.core\">AllOf</a>
</dt>"))
    (should
     (string= "./org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)"
              (jdl/extract-url entry)))))

;; (ert-deftest test-jdl/create-entry-key-value-pair ()
;;   (should
;;    (equal
;;     (cons "AllOf(Iterable<Matcher<? super T>>)"
;;           (cons "org.hamcrest.core.AllOf"
;;                 "org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)"))
;;     (jdl/create-entry-key-value-pair
;;      '("AllOf(Iterable<Matcher<? super T>>)"
;;        "org.hamcrest.core.AllOf"
;;        "org/hamcrest/core/AllOf.html#AllOf(java.lang.Iterable)")))))

;; method => '((class1 . url1) (class2 . url2)) javadoc-method-lookup,
;; length of url is 2,
;; url=(nil
;;      file://e:/java/junit4.7/javadoc/org/hamcrest/core/IsAnything.html#any(java.lang.Class))
(ert-deftest test-jdl/parse-index-all-html-no-nil-entry ()
  (let ((tbl (jdl/parse-index-all-html
              "test/hamcrest-index-all.html")))
    (loop for key being the hash-keys in tbl
          for value = (gethash key tbl)
          do (progn
               (should (not (null key)))
               (should (not (string= "nil" key)))
               (should (not (null value)))
               (loop for e in value
                     do (progn
                          (should
                           (not (null (car e))))
                          (should
                           (not (string= "nil" (car e))))
                          (should
                           (not (null (cdr e))))
                          (should
                           (not (string= "nil" (cdr e))))))))))

(ert-deftest test-jdl/parse-index-all-html-check-last-entry ()
  (let ((tbl (jdl/parse-index-all-html
              "test/hamcrest-index-all.html")))
    (should
     (not (null
           (gethash "_dont_implement_Matcher___instead_extend_BaseMatcher_()"
                    tbl)))))
  (let ((tbl (jdl/parse-index-all-html
              "test/jmock-index-all.html")))
    (should
     (not (null
           (gethash "withNoArguments()" tbl))))
    (should
     (= 1
        (length (gethash "withNoArguments()" tbl))))))

(ert-deftest test-jdl/get-url-by-method ()
  (let* ((html "test/hamcrest-index-all.html")
         (table-lst (list (jdl/parse-index-all-html html))))
    (should
     (equal
      '("file://e:/cygwin/home/whitypig/repos/git_repos/javadoc-lookup/test/org/hamcrest/StringDescription.html#toString()")
      (jdl/get-url-by-method "toString()"
                             "org.hamcrest.StringDescription"
                             table-lst)))))

(ert-deftest test-jdl/parse-index-all-html-jdk-index ()
  (let ((tbl (jdl/parse-index-all-html
              "test/jdk-index-7.html"
              :method-func #'jdl/extract-jdk-method-name
              :class-func #'jdl/extract-jdk-class-name
              :url-func #'jdl/extract-jdk-url)))
    (should
     (hash-table-p tbl))
    (should (< 0 (length (hash-table-keys tbl))))))

(ert-deftest test-jdl/extract-jdk-method-name ()
  (let ((entry
         "<dt><span class=\"strong\"><a href=\"../java/awt/Component.html#getWidth()\">getWidth()</a></span> - Method in class java.awt.<a href=\"../java/awt/Component.html\" title=\"class in java.awt\">Component</a></dt>"))
    (should
     (string=
      "getWidth()"
      (jdl/extract-jdk-method-name entry)))))

(ert-deftest test-jdl/extract-jdk-class-name ()
  (let ((entry
         "<dt><span class=\"strong\"><a href=\"../java/awt/Component.html#getWidth()\">getWidth()</a></span> - Method in class java.awt.<a href=\"../java/awt/Component.html\" title=\"class in java.awt\">Component</a></dt>"))
    (should
     (string=
      "java.awt.Component"
      (jdl/extract-jdk-class-name entry)))))

(ert-deftest test-jdl/extract-jdk-url ()
  (let ((entry
         "<dt><span class=\"strong\"><a href=\"../java/awt/Component.html#getWidth()\">getWidth()</a></span> - Method in class java.awt.<a href=\"../java/awt/Component.html\" title=\"class in java.awt\">Component</a></dt>"))
    (should
     (string=
      "../java/awt/Component.html#getWidth()"
      (jdl/extract-jdk-url entry)))))


(ert-deftest test-jdl/build-method-table ()
  (let ((jdk-table (jdl/build-method-table
                    "e:/java/jdk_7u9_docs/api")))
    (should
     (hash-table-p jdk-table))
    (should
     (< 0 (length (gethash "getWidth()" jdk-table))))
    (print (gethash "getWidth()" jdk-table))))

(ert-deftest test-jdl/get-method-cache-name ()
  (should
   (string=
    (concat
     "e:/cygwin/home/whitypig/.emacs.d/javadoc-cache/e++java+docs+hamcrest-all-1.3+.method"
     jdl/cache-version)
    (jdl/get-method-cache-filename "e:/java/docs/hamcrest-all-1.3/"))))

(ert-deftest test-jdl/save-and-load-method-cache ()
  (let* ((tbl (jdl/parse-index-all-html
               "e:/java/docs/hamcrest-all-1.3/index-all.html"))
         (cache-filename (jdl/get-method-cache-filename
                          "e:/java/docs/hamcrest-all-1.3/"))
         (jdl/method-table-lst nil))
    (should
     (not (null (gethash "toString()" tbl))))
    (should (= 0 (length jdl/method-table-lst)))
    ;; Save cache of table
    (jdl/save-cache cache-filename tbl)
    (setq tbl nil)
    (should (null (hash-table-p tbl)))
    ;; Load table from cache
    (jdl/load-method-cache cache-filename)
    (should (= 1 (length jdl/method-table-lst)))
    (setq tbl (car jdl/method-table-lst))
    (should (not (null (gethash "toString()" tbl))))))