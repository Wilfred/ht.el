;;; hm.el --- The missing hashmap library for Emacs

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: hash table, hash map, hash

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The missing hashmap utility library for Emacs.
;;
;; See documentation on https://github.com/Wilfred/hm.el

;;; Todo:

;; * Unit tests
;; * Use the terminology 'hash table' everywhere, to be consistent with Emacs built-ins.

;;; Code:

(defun hm-create ()
  "Create an empty hash table."
  (make-hash-table :test 'equal))

(defun hm-get (table key &optional default)
  "Look up KEY in TABLE, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (gethash key table default))

(defun hm-set (table key value)
  "Associate KEY in TABLE with VALUE."
  (puthash key value table)
  nil)

(defun hm-remove (table key)
  "Remove KEY from TABLE."
  (remhash key table))

(defun hm-clear (table)
  "Remove all keys from TABLE."
  (clrhash table)
  nil)

(defun hm-keys (table)
  "Return a list of all the keys in TABLE."
  (let ((keys))
    (maphash (lambda (key value) (setq keys (cons key keys))) table)
    keys))

(defun hm-values (table)
  "Return a list of all the values in TABLE."
  (let ((values))
    (maphash (lambda (key value) (setq values (cons value values))) table)
    values))

(defun hm-items (table)
  "Return a list of two-element lists '(key value) from TABLE."
    (let ((items))
    (maphash (lambda (key value) (setq items (cons (list key value) items))) table)
    items))

(provide 'hm)
;;; hm.el ends here
