;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el" (:report-format 'simplecov) (:send-report nil)))
