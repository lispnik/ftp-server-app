;;;; tools/icon.lisp -- the application icon, drawn with the objc bindings.
;;;;
;;;; A build tool, not part of the application: `make icon' loads it, and it
;;;; never enters the saved image.  The artwork is this file; res/icon.png is
;;;; what it draws, checked in so that building the bundle needs no display.
;;;;
;;;; The picture: a folder, for what is shared, with an arrow up and an arrow
;;;; down on it, for what FTP does with it.

(defpackage #:ftp-server-icon
  (:use #:common-lisp)
  (:export #:render-icon #:main))

(in-package #:ftp-server-icon)

(defparameter +appkit+
  "/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit")

(defun color (red green blue &optional (alpha 1))
  (objc:invoke "NSColor" "colorWithSRGBRed:green:blue:alpha:"
               (float red 1d0) (float green 1d0) (float blue 1d0) (float alpha 1d0)))

(defun rect (x y width height)
  (vector (float x 1d0) (float y 1d0) (float width 1d0) (float height 1d0)))

(defun point (x y)
  (vector (float x 1d0) (float y 1d0)))

(defun rounded-rect (x y width height radius)
  (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
               (rect x y width height) (float radius 1d0) (float radius 1d0)))

(defun fill-gradient (path top bottom)
  "Fill PATH with a gradient from TOP down to BOTTOM."
  (let ((gradient (objc:invoke (objc:invoke "NSGradient" "alloc")
                               "initWithStartingColor:endingColor:" bottom top)))
    ;; Ninety degrees is upwards, so the starting colour is the bottom one.
    (objc:invoke gradient "drawInBezierPath:angle:" path 90d0)
    (objc:release gradient)))

(defun arrow (x y length width head direction)
  "A filled arrow whose shaft is centred on X, starting at Y, LENGTH long and
WIDTH thick, with a head HEAD wide.  DIRECTION is 1 for up and -1 for down."
  (let ((path (objc:invoke "NSBezierPath" "bezierPath"))
        (half (/ width 2))
        (wing (/ head 2))
        (neck (+ y (* direction (- length (* 0.55 head)))))
        (tip (+ y (* direction length))))
    (objc:invoke path "moveToPoint:" (point (- x half) y))
    (objc:invoke path "lineToPoint:" (point (- x half) neck))
    (objc:invoke path "lineToPoint:" (point (- x wing) neck))
    (objc:invoke path "lineToPoint:" (point x tip))
    (objc:invoke path "lineToPoint:" (point (+ x wing) neck))
    (objc:invoke path "lineToPoint:" (point (+ x half) neck))
    (objc:invoke path "lineToPoint:" (point (+ x half) y))
    (objc:invoke path "closePath")
    ;; Rounded joins, so that the points survive being made sixteen pixels wide.
    (objc:invoke path "setLineJoinStyle:" 1)
    (objc:invoke path "setLineWidth:" (float (* 0.5 width) 1d0))
    (objc:invoke path "fill")
    (objc:invoke path "stroke")))

(defun draw-icon (size)
  "Draw the icon into the current context, SIZE points square."
  (let* ((unit (/ size 1024d0))
         (blue-top (color 0.25 0.60 0.98))
         (blue-bottom (color 0.05 0.33 0.80))
         (paper-top (color 1 1 1))
         (paper-bottom (color 0.86 0.92 1.0)))
    (flet ((u (value) (* unit value)))
      ;; The tile: the rounded square every macOS icon sits in, with the margin
      ;; the system's own icons leave round it.
      (fill-gradient (rounded-rect (u 100) (u 100) (u 824) (u 824) (u 185))
                     blue-top blue-bottom)
      ;; The folder: its tab, then its body over the tab's lower half.
      (objc:invoke (color 0.80 0.88 1.0) "setFill")
      (objc:invoke (rounded-rect (u 232) (u 560) (u 270) (u 170) (u 44)) "fill")
      (fill-gradient (rounded-rect (u 232) (u 280) (u 560) (u 390) (u 52))
                     paper-top paper-bottom)
      ;; The arrows: up for what is sent, down for what is fetched.
      (objc:invoke blue-bottom "set")
      (arrow (u 430) (u 352) (u 250) (u 52) (u 150) 1)
      (arrow (u 594) (u 602) (u 250) (u 52) (u 150) -1))))

(defun render-icon (path &optional (size 1024))
  "Write the icon to PATH as a PNG, SIZE pixels square."
  (objc:ensure-objc-initialized :modules (list +appkit+))
  (objc:with-autorelease-pool ()
    (let ((rep (objc:invoke (objc:invoke "NSBitmapImageRep" "alloc")
                            "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"
                            (cffi:null-pointer) size size 8 4 t nil
                            "NSCalibratedRGBColorSpace" 0 0)))
      ;; Pushed and popped: NSColor and NSBezierPath draw into whichever context
      ;; is current, and one left current would catch the next drawing.
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (unwind-protect
           (progn
             (objc:invoke "NSGraphicsContext" "setCurrentContext:"
                          (objc:invoke "NSGraphicsContext"
                                       "graphicsContextWithBitmapImageRep:" rep))
             (draw-icon size))
        (objc:invoke "NSGraphicsContext" "restoreGraphicsState"))
      (let ((data (objc:invoke rep "representationUsingType:properties:" 4
                               (objc:invoke "NSDictionary" "dictionary"))))
        (ensure-directories-exist path)
        (unless (objc:invoke-bool data "writeToFile:atomically:" (namestring path) t)
          (error "Could not write ~a." path)))
      (objc:release rep)
      path)))

(defun main ()
  (format t "~&wrote ~a~%" (render-icon (merge-pathnames "res/icon.png" (uiop:getcwd)))))
