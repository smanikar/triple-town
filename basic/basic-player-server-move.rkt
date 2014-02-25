#lang racket

(require web-server/servlet-env
         "basic-player-functions.rkt")

(serve/servlet move-server #:port 8080
               #:servlet-path "/move")

;servlet-regex