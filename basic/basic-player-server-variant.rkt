#lang racket

(require web-server/servlet-env
         "basic-player-functions.rkt")

(serve/servlet variant-server #:port 8080
              #:servlet-path "/variant")
