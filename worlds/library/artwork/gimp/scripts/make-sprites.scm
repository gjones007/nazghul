(define (image-set-visible-layers image selection)

  ;; Get the vector of layers
  (let ((layers (cadr (gimp-image-get-layers image)))
	)
    
    ;; Set all layers to invisible
    (map (lambda (l) (gimp-drawable-set-visible l 0)) (vector->list layers))

    ;; Set selected layers to visible
    (for-each (lambda (s)
		(gimp-drawable-set-visible (vector-ref layers s) 1))
	      selection)
    ))


(define (image-outline image)

  (let* (
	 ;; Name the foreground layer.
	 (foreground (car (gimp-image-get-active-layer image)))

	 ;; Create a new layer to hold the outline
	 (outline (car (gimp-layer-new-from-drawable foreground image)))

	 )

    ;; Add the layer at the back
    (gimp-image-add-layer image outline 2)

    ;; Fill it with black
    (gimp-drawable-fill outline FOREGROUND-FILL)

    ;; Unselect all
    (gimp-selection-none image)

    ;; Copy the image foreground alpha to the selection buffer
    (gimp-selection-layer-alpha foreground)

    ;; Grow the selection by 8 pixels
    (gimp-selection-grow image 8)

    ;; Invert the selection
    (gimp-selection-invert image)

    ;; Clear the selection
    (gimp-edit-clear outline)

    ;; Unselect all
    (gimp-selection-none image)

    ;; Return the resulting outline layer
    outline
    ))


(define (make-frame image layers)

  (let* (
	 ;; Make a new image for the frame
	 (frame (car (gimp-image-new 128 128 RGB)))
	 )

    ;; Make only the layers for the frame visible
    (image-set-visible-layers image layers)
  
    (let (
	  ;; Make a new layer from the visible layers
	  (merged (car (gimp-layer-new-from-visible image frame "merged")))
	  )
      
      ;; Add the merged layers to the new frame image
      (gimp-image-add-layer frame merged 0)

      ;; Add the outline layer
      (image-outline frame)

      ;; Return the new frame image
      frame
      )
    ))

(define (append-frame sprite frame position)
    
    ;; Scale the frame using the best-but-slowest interpolation (cubic)
    (gimp-image-scale-full frame 32 32 INTERPOLATION-CUBIC)
    
    (let (
	  ;; Copy the frame
	  (final (car (gimp-layer-new-from-visible frame sprite "frame")))
	  )

      ;; Add it to the final sprite buffer
      (gimp-image-add-layer sprite final 0)

      ;; Move it into position
      (gimp-layer-translate final (* position 32) 0)

      ))


(define (make-sprite in-file out-file)
  (let* (
	 ;; Load the file
	 (image (car (gimp-file-load RUN-NONINTERACTIVE in-file in-file)))
	 
	 ;; Create a destination image to hold the 4 final frames
	 (sprite (car (gimp-image-new (* 4 32) 32 RGB)))
	 )
    
    ;; For each frame, extract the necessary layers, outline them, shrink them,
    ;; and append them to the final sprite image.
    (for-each (lambda (frame)
    		(append-frame sprite
    			      (make-frame image (cdr frame))
    			      (car frame)))
    	      (list
    	       (list 0  0 1 2 3  10 11  12 13 14 15 16 17 18 19 20)
    	       (list 1  0 1 2 3  8 9    12 13 14 15 16 17 18 19 20)
    	       (list 2  4 5 6 7  8 9    12 13 14 15 16 17 18 19 20)
    	       (list 3  4 5 6 7  10 11  12 13 14 15 16 17 18 19 20)
	       ))

    (gimp-selection-none sprite)

    ;; Save the resulting frames
    (gimp-file-save RUN-NONINTERACTIVE sprite
		    (car (gimp-image-merge-visible-layers sprite CLIP-TO-IMAGE))
		    out-file out-file)

    ))