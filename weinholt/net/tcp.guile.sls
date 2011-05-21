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

;; tcp-connect for GNU Guile. Tested with Guile 2.0.1.

(library (weinholt net tcp (0 0 20110521))
  (export tcp-connect)
  (import (rnrs)
          (guile))

  ;; Returns: input-port output-port
  (define (tcp-connect host service)
    (let lp ((addrs (catch 'getaddrinfo-error
                      (lambda ()
                        (getaddrinfo host service 0 0 SOCK_STREAM))
                      (lambda (key errcode)
                        (error 'tcp-connect (gai-strerror errcode)
                               host service)))))
      (if (null? addrs)
          (error 'tcp-connect "Could not connect" host service)
          (let* ((addr (car addrs))
                 (s (socket (addrinfo:fam addr)
                            (addrinfo:socktype addr)
                            (addrinfo:protocol addr))))
            (catch 'system-error
                   (lambda ()
                     (connect s (addrinfo:addr addr))
                     (values s s))
                   (lambda (key subr message args . rest)
                     (if (null? (cdr addrs))
                         (error 'tcp-connect (apply format #f message args)
                                host service)
                         (lp (cdr addrs))))))))))
