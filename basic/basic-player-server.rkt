#lang racket

(require web-server/servlet-env
         "basic-player-functions.rkt")

(serve/servlet server/player #:port 8080
               #:servlet-regexp (regexp ".*"))