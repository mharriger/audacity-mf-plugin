;nyquist plug-in
;version 4
;type generate
;name "MF..."
;action "Generating MF..."
;author "Matt Harriger"
;copyright "Copyright 2017 Matt Harriger. Released under the terms of the MIT license."

;control mfstring "MF String" string "0-9, KP, KP', ST, ST', ST''" "123456"
;control volume "Volume" real "linear" 0.3 0 1
;control env "Envelope" real "linear" 0.002 0 .01

; MF Tone Generator
; by Matt Harriger
; November, 2017
; Inspired by this thread: https://forum.audacityteam.org/viewtopic.php?f=39&t=70946 

; Tone Length. 68ms is standard
(setf tlen .068)
; KP Tone Length. 100ms is standard
(setf kplen .1)
; Tone spacing. 68ms is standard
(setf ts .068)

(setf tonepairs  '(
                   ("1" . (700 900))
                   ("2" . (700 1100))
                   ("3" . (900 1100))
                   ("4" . (700 1300))
                   ("5" . (900 1300))
                   ("6" . (1100 1300))
                   ("7" . (700 1500))
                   ("8" . (900 1500))
                   ("9" . (1100 1500))
                   ("0" . (1300 1500))
                   ("ST''" . (700 1700))
                   ("ST'" . (900 1700))
                   ("KP"  . (1100 1700))
                   ("KP'" . (1300 1700))
                   ("ST"  . (1500 1700))))


(defun gen-mf (digit volume tlen ts)
  ;If KP or KP2 use kplen for tone length, else use tlen
  (if (string-equal (subseq digit 0 1) "K") (setf len kplen) (setf len tlen))
  ;Get low and high values from a-list
  (setf low (second (assoc digit tonepairs :test 'equal)))
  (setf high (third (assoc digit tonepairs :test 'equal)))
  ;Generate the tone
  (print (strcat digit " : " (format nil "~A" len) " " (format nil "~A" low) " " (format nil "~A" high)))
  (seq
    (mult volume
          ;Envelope
          (pwl env 1 (- len env) 1 len)
          ;Generate high and low tones
          (sim
            (osc (hz-to-step high) len)
            (osc (hz-to-step low) len)))
    ;Inter-digital spacing
    (s-rest ts)))

;Buffer to hold non-numeric digits (KP, ST, etc.)
(setf buf "")
;List of strings to represent MF digits
(setf mfseq (list))
;Parse the input string one charater at a time
(dotimes (n (length mfstring))
        (setf c (char mfstring n))
        (if (digit-char-p c)
             ;If the character is a digit
             (progn
                 ;If there is something in the buffer, add it to the list of MF digits
                 (if (> (length buf) 0) (progn (setf mfseq (append mfseq (list buf))) (setf buf "")))
                 ;Add the numeric digit to the list of MF digits
                 (setf mfseq (append mfseq (list (format nil "~A" c)))))
             ;If the character is not a numeric digit
             (if (and (or (eql c #\K) (eql c #\S)) (> (length buf) 0))
                ;If the character is a K or S, and there is currently something in the buffer, add buffer to list of MF digits
                (progn (setf mfseq (append mfseq (list buf))) (setf buf "") (setf buf (strcat buf (format nil "~A" c))))
                ;Otherwise add the character to the buffer
                (setf buf (strcat buf (format nil "~A" c))))))
;If there's anything in the buffer, add it to the list of MF digits
(if (> (length buf) 0) (setf mfseq (append mfseq (list buf))))

;Generate the audio
(seqrep ( i (length mfseq))(gen-mf (nth i mfseq) volume tlen ts))

