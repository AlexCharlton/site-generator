#!/bin/bash
quicklispDir=~/quicklisp/

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

CC=gcc \
buildapp --eval "(load \"${quicklispDir}/setup.lisp\")" \
         --asdf-path $dir/ \
         --eval "(ql:quickload :site-generator)" \
         --eval "(defun debug-ignore (c h) (declare (ignore h)) (format t \"~a~%\" c) (abort))" \
         --eval "(setf sb-ext:*invoke-debugger-hook* #'debug-ignore)" \
         --entry sg::main \
         --compress-core \
         --output site-generator
