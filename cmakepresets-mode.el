;;; cmakepresets-mode.el --- Major mode for CMake presets file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Moreira

;; Author: Moreira <AUTH\moreirad@lx-3117035b368e>
;; Version: 0.1
;; Package-Requires: ((json "1.5"))
;; Keywords: c c++ cmake languages tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a major mode for CMake presets files, which includes imenu
;; support.

;;; Code:

(require 'json)

(defun cmakepresets-imenu-create-index ()
  "Create an imenu index for CMake presets files."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (index-alist '())
        (preset-types '(("configurePresets" . "Configure")
                        ("buildPresets" . "Build")
                        ("testPresets" . "Test")
                        ("packagePresets" . "Package")
                        ("workflowPresets" . "Workflow"))))
    (save-excursion
      (goto-char (point-min))
      (let ((json-data (condition-case nil
                           (json-read)
                         (error nil))))
        (when (and json-data (listp json-data))
          (dolist (preset-type preset-types)
            (let ((type-key (car preset-type))
                  (type-name (cdr preset-type)))
              (when-let* ((presets (assoc-default type-key json-data)))
                (dolist (preset presets)
                  (when-let* ((name (assoc-default "name" preset)))
                    (let ((entry-name (format "%s/%s" type-name name))
                          (entry-pos (save-excursion
                                       (goto-char (point-min))
                                       (search-forward type-key nil t)
                                       (search-forward (format "\"name\": \"%s\"" name) nil t))))
                      (when entry-pos
                        (push (cons entry-name entry-pos) index-alist)))))))))))
    (nreverse index-alist)))


(defun cmakepresets-imenu-create-index-with-groups ()
  "Create an imenu index for CMake presets files with Consult groups."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (index-alist '())
        (preset-types '(("configurePresets" . "Configure")
                        ("buildPresets" . "Build")
                        ("testPresets" . "Test")
                        ("packagePresets" . "Package")
                        ("workflowPresets" . "Workflow"))))
    (save-excursion
      (goto-char (point-min))
      (let ((json-data (condition-case nil
                           (json-read)
                         (error nil))))
        (when (and json-data (listp json-data))
          (dolist (preset-type preset-types)
            (let ((type-key (car preset-type))
                  (type-name (cdr preset-type))
                  (group-alist '()))
              (when-let* ((presets (assoc-default type-key json-data)))
                (dolist (preset presets)
                  (when-let* ((name (assoc-default "name" preset)))
                    (let ((entry-name (format "%s/%s" type-name name))
                          (entry-pos (save-excursion
                                       (goto-char (point-min))
                                       (search-forward type-key nil t)
                                       (search-forward (format "\"name\": \"%s\"" name) nil t))))
                      (when entry-pos
                        (push (cons entry-name entry-pos) group-alist))))))
              (when group-alist
                (push (cons type-name (nreverse group-alist)) index-alist)))))))
    (nreverse index-alist)))


(defun cmakepresets-setup-imenu ()
  "Set up imenu for CMake presets files."
  ;; (setq-local imenu-create-index-function #'cmakepresets-imenu-create-index)
  (setq-local imenu-create-index-function #'cmakepresets-imenu-create-index-with-groups)
  )


;;;###autoload
(define-derived-mode cmakepresets-mode json-ts-mode "CMakePresets"
  "Major mode for editing CMakePresets.json files.

This mode inherits from `json-mode` and provides additional functionality
specific to CMake presets, such as imenu support."
  (cmakepresets-setup-imenu))


;;;###autoload
(add-to-list 'auto-mode-alist '("CMakePresets\\.json\\'" . cmakepresets-mode))
(add-to-list 'auto-mode-alist '("CMakeUserPresets\\.json\\'" . cmakepresets-mode))


(provide 'cmakepresets-mode)
;;; cmakepresets-mode.el ends here
