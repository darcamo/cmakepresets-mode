;;; cmakepresets-mode.el --- Major mode for CMake presets file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Moreira

;; Author: Moreira <AUTH\moreirad@lx-3117035b368e>
;; Version: 0.1
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
(require 'cmakepresets-core)
(require 'cmakepresets-completions)


;;;###autoload
(define-derived-mode cp-mode json-ts-mode "CMakePresets"
  "Major mode for editing CMakePresets.json files.

This mode inherits from `json-mode` and provides additional functionality
specific to CMake presets, such as imenu support."
  (cp-setup-imenu))


;;;###autoload
(add-to-list 'auto-mode-alist '("CMakePresets\\.json\\'" . cp-mode))
(add-to-list 'auto-mode-alist '("CMakeUserPresets\\.json\\'" . cp-mode))


;; Completions
(add-hook 'cmakepresets-mode-hook #'cp-mode-setup-completions)


(provide 'cmakepresets-mode)
;;; cmakepresets-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cp-" . "cmakepresets-"))
;; End:
