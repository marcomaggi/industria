;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

(library (weinholt net tcp)
  (export tcp-connect)
  (import (rnrs)
          (primitives r5rs:require get-service-by-name make-client-socket
                      socket-input-port socket-output-port))

  (define (tcp-connect host service)
    (let ((port (or (string->number service 10)
                    (let-values (((port . _) (get-service-by-name service)))
                      port))))
      (let ((s (make-client-socket host port)))
        (values (socket-input-port s)
                (socket-output-port s)))))

  (r5rs:require 'socket))
