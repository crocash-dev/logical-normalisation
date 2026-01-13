(defpackage #:logical-normalisation-system-pkg (:use :asdf :cl))
(in-package #:logical-normalisation-system-pkg)

(defsystem logical-normalisation
  :name "logical-normalisation"
  :author "Joseph Crockett Ashford"
  :version "0.1"
  :maintainer "Joseph Crockett Ashford"
  :licence "BSD"
  :description "Reduce logical formula to DNF or CNF"
  :long-description ""
  :components
  ((:file "logical-normalisation"))
  :depends-on (:iterate :cl-ppcre :split-sequence))
