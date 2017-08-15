(load-file "package+.el")

(setq package-archives (remove (assoc "gnu" package-archives) package-archives))
(dolist (repo '(("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives repo))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'ert)

(local-set-key (kbd "C-c C-r")
               (lambda ()
                 (interactive)
                 (ert-delete-all-tests)
                 (eval-buffer)
                 (ert-run-tests-interactively t)))

(define-key ert-results-mode-map (kbd "C-c C-r") 'ert-results-rerun-all-tests)

;; TODO
;; (defun with-known-packages (body)
;;   (unwind-protect
;;       (progn [set up]
;;              (funcall body))
;;     [tear down]))

(ert-deftest test-package-cleanup ()
  (ert-skip 'destructive))

(ert-deftest test-package-delete-by-name ()
  (ert-skip 'destructive))

(ert-deftest test-package-deps-for ()
  (should (equal (package-deps-for 'emacs)
                 nil))
  (should (equal (package-deps-for 'racket-mode) ; FIX
                 '((emacs (24 3)) (faceup (0 0 2)) (s (1 9 0))))))

(ert-deftest test-package-details-for ()
  (should (equal (package-details-for 'emacs)
                 nil))
  (should (equal (package-details-for 'racket-mode)
                 (car (alist-get 'racket-mode package-alist)))))

(defun assoc-many (keys alist)
  (mapcar (lambda (key) (or (assoc key alist)
                            (list key)))
          keys))

(ert-deftest test-package-installed-with-deps/ag ()
  (should (equal (package-installed-with-deps
                  (assoc-many '(ag cl-lib dash s) package-alist))
                 '((ag cl-lib dash s)
                   (dash)
                   (s)))))

(ert-deftest test-package-installed-with-deps/gh ()
  (should (equal (package-installed-with-deps
                  (assoc-many '(dash emacs gh logito marshal ht pcache s) package-alist))
                 '((dash)
                   (gh dash emacs logito marshal pcache s)
                   (ht dash)
                   (logito eieio)
                   (marshal eieio ht json)
                   (pcache eieio)
                   (s)))))

(ert-deftest test-package-installed-with-deps/ag+gh ()
  (should (equal (package-installed-with-deps
                  (assoc-many '(ag cl-lib dash gh emacs ht logito marshal pcache s)
                              package-alist))
                 '((ag cl-lib dash s)
                   (dash)
                   (gh dash emacs logito marshal pcache s)
                   (ht dash)
                   (logito eieio)
                   (marshal eieio ht json)
                   (pcache eieio)
                   (s)))))

(ert-deftest test-package-installed-with-deps/all ()
  (should (equal (length
                  (seq-intersection (package-installed-with-deps)
                                    '((ag cl-lib dash s)
                                      (dash)
                                      (s))))
                 3)))

(ert-deftest test-package-manifest ()
  (ert-skip 'need-to-learn-stubbing))

(ert-deftest test-package-manifest-with-deps/ag ()
  (should (equal (package-manifest-with-deps '(ag))
                 '((ag cl-lib dash s)
                   (dash)
                   (s)))))

(ert-deftest test-package-manifest-with-deps/gh ()
  (should (equal (package-manifest-with-deps '(gh))
                 '((dash)
                   (gh dash emacs logito marshal pcache s)
                   (ht dash)
                   (logito eieio)
                   (marshal eieio ht json)
                   (pcache eieio)
                   (s)))))

(ert-deftest test-package-manifest-with-deps/ag+gh ()
  (should (equal (package-manifest-with-deps '(ag gh))
                 (sort (cl-remove-duplicates
                        (cl-union (package-manifest-with-deps '(ag))
                                  (package-manifest-with-deps '(gh)))
                        :test 'equal)
                       'symbol-list<))))

(package-manifest-with-deps '(gh))
;; (package-manifest-with-deps '(ag))
;; (package-manifest-with-deps '(ag gh))

(ert-deftest test-package-maybe-install ()
  (ert-skip 'need-to-learn-stubbing))

(ert-deftest test-package-transitive-closure/old ()
  :expected-result :failed
  (should (equal (package-transitive-closure/old '(gh))
                 '(emacs s dash eieio pcache logito json ht marshal gh))))

;; TODO: standardize on something cleaner but still buggy

;; TODO: learn to write test: w/ -> being topo + with-deps
;; have: a b c -> a b c d
;; want: a b   -> a b   d

(ert-deftest test-package-transitive-closure/ag ()
  (should (equal (package-transitive-closure '(ag))
                 '(cl-lib dash s ag))))

(ert-deftest test-package-transitive-closure/gh ()
  (should (equal (package-transitive-closure '(gh))
                 '(dash emacs s eieio logito pcache ht json marshal gh))))

(ert-deftest test-package-transitive-closure/ag+gh ()
  (should (equal (package-transitive-closure '(ag gh))
                 '(cl-lib dash s ag emacs eieio logito pcache ht json marshal gh))))

(ert-deftest test-package-version-for ()
  (should (equal '(20170617 1942)       ; FIX this will fail quickly w/o fixtures
                 (package-version-for 'racket-mode))))

(ert-deftest test-topological-sort ()
  (setq deps '((ag cl-lib dash s)
               (dash)
               (gh dash emacs logito marshal pcache s)
               (ht dash)
               (logito eieio)
               (marshal eieio ht json)
               (pcache eieio)
               (s)))
  (should (equal (car (topological-sort deps))
                 '(cl-lib dash ht s ag emacs eieio logito pcache json marshal gh))))

(ert-deftest test-symbol< ()
  (should (equal (sort '(z y x) 'symbol<)
                 '(x y z))))

(ert-deftest test-symbol-list< ()
  (should (equal (sort '((z y x) (y x w) (x w v)) 'symbol-list<)
                 '((x w v) (y x w) (z y x)))))
