;;; melpazoid-flymake.el --- Flymake backend with melpazoid -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/melpazoid-flymake
;; Version: 0.1.0
;; Keywords: docs
;; Package-Requires: ((emacs "28.1") (flymake "1.2.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend with melpazoid

;;; Code:

(require 'flymake)
(require 'url-parse)
(require 'find-func)

(defcustom melpazoid-flymake-directory nil
  "Melpazoid directory."
  :group 'melpazoid
  :type 'directory)

(defvar melpazoid-flymake-github-workflow-template nil)

(defun melpazoid-flymake-lint-util-ssh-to-https (ssh-remote)
  "Convert SSH-REMOTE to https url."
  (with-temp-buffer
    (save-excursion
      (insert ssh-remote))
    (when (re-search-forward "@" nil t 1)
      (when-let* ((beg (point))
                  (end (re-search-forward ":" nil t 1)))
        (string-trim
         (concat "https://"
                 (buffer-substring-no-properties
                  beg (1- end))
                 "/"
                 (buffer-substring-no-properties
                  end (point-max))))))))

(defun melpazoid-flymake-lint-util-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let* ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil
                                  '("remote" "-v")))
                  (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l)
               (let ((parts (split-string l)))
                 (cons (car parts)
                       (cadr parts))))
             (split-string remotes "\n" t)))))



(defun melpazoid-flymake-get-recipe-plist ()
  "Return plist of current git repo as recipe :repo, :type and :host."
  (when-let* ((url (cdar (melpazoid-flymake-lint-util-remotes-alist)))
              (urlobj (url-generic-parse-url
                       (or (melpazoid-flymake-lint-util-ssh-to-https url)
                           url)))
              (host (url-host urlobj))
              (filename (url-filename urlobj)))
    (list
     :repo
     (replace-regexp-in-string
      "^/\\|[\\.]git$" "" filename)
     :type "git"
     :host
     (file-name-base host))))

(defun melpazoid-flymake--provided-feature ()
  "Return the last-provided feature name, as a string, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (cond ((re-search-backward
            (rx "(provide '"
                (group (1+ (or (syntax word)
                               (syntax symbol)))))
            nil t)
           (match-string-no-properties 1))
          ((re-search-backward "(provide-me)" nil t)
           (file-name-sans-extension
            (file-name-nondirectory buffer-file-name))))))


(defun melpazoid-flymake-find-package-name (directory &optional recipe)
  "Extract package name from Emacs Lisp files.

Argument DIRECTORY is the directory to search for Emacs Lisp files.

Optional argument RECIPE is a plist containing package metadata, which may
include the repository URL."
  (let* ((files (directory-files (project-root (project-current nil directory))
                                 t
                                 directory-files-no-dot-files-regexp))
         (el-files (seq-filter (lambda (it)
                                 (equal (file-name-extension it) "el"))
                               files))
         (cands))
    (dolist (file el-files)
      (with-temp-buffer
        (erase-buffer)
        (let (emacs-lisp-mode-hook)
          (emacs-lisp-mode))
        (progn
          (insert-file-contents file)
          (when-let* ((provided
                      (melpazoid-flymake--provided-feature)))
            (push provided cands)))))
    (cond ((not cands)
           (let ((name
                  (if-let* ((repo (plist-get recipe :repo)))
                      (car (last (split-string repo "/" t)))
                    (file-name-nondirectory (directory-file-name directory)))))
             (read-string "Package name: " name)))
          ((length= cands 1)
           (car cands))
          (t (completing-read "Package name: " cands)))))

(defun melpazoid-flymake-get-recipe ()
  "Return recipe for current repo."
  (let ((name (melpazoid-flymake--provided-feature))
        (recipe
         (melpazoid-flymake-get-recipe-plist)))
    (list name
          :fetcher (plist-get recipe :host)
          :repo (prin1-to-string (plist-get recipe :repo)))))

(defun melpazoid-flymake-get-melpa-recipe-in-dir (directory &optional
                                                            package-name)
  "Return recipe for PACKAGE-NAME in DIRECTORY.
Recipe is a list, e.g. (PACKAGE-NAME :repo \"owner/repo\" :fetcher github)."
  (let* ((recipe
          (let ((default-directory directory))
            (melpazoid-flymake-get-recipe-plist)))
         (name (or package-name
                   (melpazoid-flymake-find-package-name directory)
                   (car (last (split-string
                               (plist-get recipe :repo) "/" t))))))
    (list (if (stringp name)
              (intern name)
            name)
          :fetcher (intern (plist-get recipe :host))
          :repo (plist-get recipe :repo))))

(defun melpazoid-flymake-download-url (url)
  "Download URL and return stirng."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (buffer-string))
      (kill-buffer download-buffer))))


;;;###autoload
(defun melpazoid-flymake-add-template (directory)
  "Download melpazoid template to .github/workflows/melpazoid.yml in DIRECTORY."
  (interactive (list (read-directory-name "Project Directory: ")))
  (let ((default-directory (expand-file-name (project-root (project-current
                                                            nil directory)))))
    (cond ((not (file-exists-p directory))
           (user-error "Directory %s doesn't exist" directory))
          (t
           (let ((dir
                  (expand-file-name ".github/workflows" directory)))
             (unless (file-exists-p dir)
               (make-directory dir 'parents))
             (let
                 ((melpazoid-str
                   (melpazoid-flymake-replace-template
                    (melpazoid-flymake-get-melpa-recipe-in-dir
                     directory)
                    nil
                    (or melpazoid-flymake-github-workflow-template
                        (setq melpazoid-flymake-github-workflow-template
                              (melpazoid-flymake-download-url
                               "https://raw.githubusercontent.com/riscy/melpazoid/master/melpazoid.yml"))))))
               (write-region melpazoid-str nil
                             (expand-file-name
                              "melpazoid.yml" dir))))))))

(defun melpazoid-flymake-replace-template (recipe exist-ok melpazoid-template)
  "Replace RECIPE and EXIST-OK in MELPAZOID-TEMPLATE."
  (with-temp-buffer
    (insert melpazoid-template)
    (when (re-search-backward "^\\([\s]+\\)?RECIPE:" nil t
                              1)
      (goto-char (match-end 0))
      (skip-chars-forward "\s\t")
      (delete-region (point)
                     (line-end-position))
      (insert (prin1-to-string recipe))
      (when (and (not exist-ok)
                 (or (re-search-forward "^\\([\s]+\\)?EXIST_OK:" nil t 1)
                     (re-search-backward "^\\([\s]+\\)?EXIST_OK:" nil t 1)))
        (skip-chars-forward "\s\t")
        (let* ((beg (point))
               (end (save-excursion
                      (+ beg (skip-chars-forward "a-z")))))
          (delete-region beg end)
          (insert "false")))
      (buffer-string))))

(defvar melpazoid-flymake--process nil)

(defun melpazoid-flymake--report-errors-by-regex (regex line-subexp text-subext
                                                        &optional dir)
  "Find errors matching REGEX in current buffer.
LINE-SUBEXP and TEXT-SUBEXT specify the matching subexpressions for line number
and error message.
Return the list of form (file line error).
If DIR is provided, expand file to absolute, using DIR as default directory."
  (let ((problems))
    (goto-char (point-max))
    (while (re-search-backward regex
                               nil
                               t 1)
      (let ((line (match-string-no-properties line-subexp))
            (text (match-string-no-properties text-subext)))
        (let ((file (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (point)))))
          (push (list
                 (if dir
                     (expand-file-name file dir)
                   file)
                 (string-to-number line)
                 (string-join (split-string text nil t) " "))
                problems))))
    problems))


(defun melpazoid-flymake--report (stdout-buffer dir)
  "Create Flymake diag messages from contents of STDOUT-BUFFER and DIR.
Return a list of results (file line text)."
  (let ((problems)
        (alist `(("\\(:\\([0-9]+\\):\\([0-9]+\\):\\(\\([^\n]+\\)[\n]\\([\s][^\n]+\\)?\\)\\)"
                  2 4 ,dir)
                 ("#L\\(\\([0-9]+\\):[\s\t]?\\([^\n]+\\)\\)"
                  2 3 ,dir))))
    (with-current-buffer stdout-buffer
      (dolist (args alist)
        (goto-char (point-max))
        (setq problems (nconc problems
                              (apply #'melpazoid-flymake--report-errors-by-regex
                                     args)))))
    problems))

(defun melpazoid-flymake-make-diag (problem)
  "Make diag from PROBLEM.
PROBLEM is a list of file, line and text."
  (pcase-let* ((`(,file ,line ,text)
                problem)
               (buff (find-file-noselect file))
               (`(,beg . ,end)
                (flymake-diag-region buff line)))
    (flymake-make-diagnostic
     buff
     beg
     end
     :error
     text)))

(defvar melpazoid-flymake-temp-directory nil)
(defun melpazoid-flymake-setup-temp-dir ()
  "Copy melpazoid dir to temporarily directory to adjust makefile."
  (unless melpazoid-flymake-temp-directory
    (setq melpazoid-flymake-temp-directory
          (concat (temporary-file-directory)
                  "melpazoid-flymake-check")))
  (let ((makefile (expand-file-name "Makefile"
                                    melpazoid-flymake-temp-directory)))
    (unless (file-exists-p (expand-file-name "Makefile"
                                             melpazoid-flymake-temp-directory))
      (copy-directory
       melpazoid-flymake-directory
       melpazoid-flymake-temp-directory)
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-max))
        (when (re-search-backward "--progress=plain " nil t 1)
          (replace-match "" nil nil nil 0))
        (write-region (buffer-string) nil makefile nil nil nil nil)))))

;;;###autoload
(defun melpazoid-flymake-compile ()
  "Run melpazoid check on current file with compile command."
  (interactive)
  (let ((dir default-directory)
        (recipe (melpazoid-flymake-get-recipe)))
    (melpazoid-flymake-setup-temp-dir)
    (let ((default-directory dir))
      (compile (format "RECIPE='%s' LOCAL_REPO='%s' make -C %s"
                       recipe
                       (shell-quote-argument dir)
                       (shell-quote-argument
                        melpazoid-flymake-temp-directory))))))

(defun melpazoid-flymake-report (callback &rest _)
  "Create melpazoid process for current buffer.
Invoke CALLBACK with flymake diagnostics."
  (when (process-live-p melpazoid-flymake--process)
    (kill-process melpazoid-flymake--process))
  (let ((process-environment
         (append process-environment
                 (list (format "RECIPE=%s"
                               (melpazoid-flymake-get-recipe))
                       (format "LOCAL_REPO=%s"
                               (shell-quote-argument default-directory)))))
        (default-directory default-directory)
        (callback callback))
    (melpazoid-flymake-setup-temp-dir)
    (setq melpazoid-flymake--process
          (make-process
           :name "melpazoid-flymake"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *melpazoid-flymake*")
           :command `("make"
                      "-C"
                      ,(shell-quote-argument
                        melpazoid-flymake-temp-directory))
           :sentinel
           (lambda (proc &rest _ignored)
             (when (and (eq 'exit (process-status proc)))
               (let* ((proc-buffer (process-buffer proc))
                      (problems (melpazoid-flymake--report
                                 proc-buffer
                                 default-directory)))
                 (funcall
                  callback
                  (mapcar #'melpazoid-flymake-make-diag
                          problems)))))))))

;;;###autoload
(defun melpazoid-flymake-enable ()
  "Enable Flymake with melpazoid-flymake backend."
  (interactive)
  (unless melpazoid-flymake-directory
    (setq melpazoid-flymake-directory
          (ignore-errors
            (when (fboundp 'file-name-parent-directory)
              (file-name-parent-directory (file-name-parent-directory
                                           (find-library-name
                                            "melpazoid")))))))
  (unless melpazoid-flymake-directory
    (user-error
     "melpazoid-flymake: `melpazoid-flymake-directory' is not set"))
  (when buffer-file-name
    (add-hook 'flymake-diagnostic-functions #'melpazoid-flymake-report
              nil t)
    (unless (bound-and-true-p flymake-mode)
      (flymake-mode 1))
    (flymake-start)))

;;;###autoload
(defun melpazoid-flymake-disable ()
  "Disable Flymake backend."
  (interactive)
  (let ((fmode (bound-and-true-p flymake-mode)))
    (progn
      (when fmode
        (flymake-mode -1))
      (remove-hook 'flymake-diagnostic-functions
                   #'melpazoid-flymake-report t)
      (when fmode
        (flymake-mode 1)))))


;;;###autoload
(define-minor-mode melpazoid-flymake-mode
  "Run flymake with melpazoid-flymake backend."
  :lighter " FlyMelpazoid"
  :global nil
  (if melpazoid-flymake-mode
      (melpazoid-flymake-enable)
    (melpazoid-flymake-disable)))

(provide 'melpazoid-flymake)
;;; melpazoid-flymake.el ends here