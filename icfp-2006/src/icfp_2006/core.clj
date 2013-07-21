(ns icfp-2006.core
  (:require [org.clojars.smee.binary.core :as b]
            [clojure.java.io :as io])
  (:gen-class))

;;; instruction definitions and manipulation
(def instructions [:cond-move
                   :array-index
                   :array-amandment
                   :add
                   :mult
                   :div
                   :nand
                   :halt
                    :alloc
                    :abandon
                    :output
                    :input
                    :load
                    :ortho])
(def instr->num (into {} (map-indexed #(vector %2 %1) instructions)))

(defn decode-opcode [instr]
  (nth instructions (bit-shift-right instr 28)))

(defn encode-opcode [instr]
  (assert false "not implemented"))

(defn decode-instruction [^long instr]
  (let [opcode (decode-opcode instr)
        a (bit-shift-right (bit-and instr 2r111000000) 6)
        b (bit-shift-right (bit-and instr 2r111000) 3)
        c (bit-and instr 2r111)
        special-a (bit-shift-right (bit-and instr 2r1110000000000000000000000000) 25)
        value (bit-and instr 2r1111111111111111111111111)]
    [opcode a b c special-a value]))

(defn pr-str-instruction [[op a b c special-a value]]
  (condp = op 
    :cond-move (str "r"a "=r"b " iff r" c "!=0")
    :ortho (str "mov r" special-a ", " value)
    :load (str "load array r"b "@r"c) 
    :halt "halt!"
    :alloc (str "alloc array with capacity r" c) 
    :abandon (str "delete array r"c)
    :output (str "print r"c)
    :input (str "read r"c) 
    (str "r" a "= r" b " " (name op) " r" c)))

(def instr-frame (b/compile-codec (b/repeated :uint-be) identity vec))

;;;;;;;;;;;; interpreter
(defn init-program [instructions]
  (let [registers (vec (repeat 8 (long 0)))
        arrays {0 instructions}
        pointer 0]
    [registers arrays pointer]))


(defn eval-instruction [[reg arr pt]]
  (let [instr (get-in arr [0 pt])
        [opcode a b c special-a value] (decode-instruction instr)
;        _ (println (pr-str-instruction [opcode a b c special-a value]) [reg pt] [a b c special-a value])
        pt (inc pt)] 
    (condp = opcode
      :cond-move (if (zero? (reg c)) 
                   [reg arr pt] 
                   [(assoc reg a (reg b)) arr pt])
      :array-index [(assoc reg a (get-in arr [(reg b) (reg c)])) arr pt] 
      :array-amandment [reg (assoc-in arr [(reg a) (reg b)] (reg c)) pt] 
      :add  [(assoc reg a (mod (+ (reg b) (reg c)) (bit-shift-left 1 32))) arr pt] 
      :mult [(assoc reg a (mod (* (reg b) (reg c)) (bit-shift-left 1 32))) arr pt]
      :div  [(assoc reg a (long (/ (bit-and (reg b) 0xffffffff) (bit-and (reg c) 0xffffffff)))) arr pt]
      :nand [(assoc reg a (bit-not (bit-and (reg b) (reg c)))) arr pt]
      :halt nil
      :alloc (let [new-idx (inc (apply max (keys arr)))] 
               [(assoc reg b new-idx) (assoc arr new-idx (vec (repeat (reg c) (long 0)))) pt]) 
      :abandon [reg (dissoc arr (reg c)) pt] 
      :output (do (print (char (reg c))) [reg arr pt]) 
      :input [(assoc reg c (.read System/in)) arr pt] 
      :load [reg (assoc arr 0 (arr (reg b))) (reg c)] 
      :ortho [(assoc reg special-a value) arr pt])))

(defonce ^:const two-exp-32 (bit-shift-left 1 32)) 

(defn eval-instructions! [^longs reg ^java.util.Map arr ^long pt]
  (let [instr (aget ^longs (.get arr 0) pt) 
        [opcode a b c special-a value] (decode-instruction instr)
;        _ (println (pr-str-instruction [opcode a b c special-a value]) [reg pt] [a b c special-a value])
        ] 
    (condp = opcode
      :cond-move (when (not (zero? (aget reg c))) 
                   (aset-long reg a (aget reg b)))
      :array-index (aset-long reg a (aget ^longs (.get arr (aget reg b)) (aget reg c))) 
      :array-amandment (aset-long ^longs (.get arr (aget reg a)) (aget reg b) (aget reg c)) 
      :add  (aset-long reg a (mod (+ (aget reg b) (aget reg c)) two-exp-32))  
      :mult (aset-long reg a (mod (* (aget reg b) (aget reg c)) two-exp-32))
      :div  (aset-long reg a (long (/ (bit-and (aget reg b) 0xffffffff) (bit-and (aget reg c) 0xffffffff))))
      :nand (aset-long reg a (bit-not (bit-and (aget reg b) (aget reg c))))
      :halt nil
      :alloc (let [new-idx (inc (apply max (.keySet arr)))
                   new-arr (long-array (aget reg c))] 
               (do (aset-long reg b new-idx) 
                 (.put arr new-idx new-arr))) 
      :abandon (.remove arr (aget reg c)) 
      :output (print (char (aget reg c))) 
      :input (aset-long reg c (.read System/in)) 
      :load (when (not (zero? (aget reg b))) #_(println (aget reg b) (alength (.get arr (aget reg b))) (aget reg c)) (.put arr 0 (.get arr (aget reg b))))
      :ortho (aset-long reg special-a value))
    (cond 
      (= opcode :halt) nil
      (= opcode :load) (recur reg arr (aget reg c))
      :else (recur reg arr (inc pt))))) 


;; I/O

(defn read-frame [in]
  (try (b/decode instr-frame in)
    (catch java.io.EOFException e nil)))

(comment
  (set! *print-length* 10)
  (set! *print-level* 3)
  (with-open [in (io/input-stream "c:\\Temp\\icfp2006\\sandmark.umz ")] 
    (dorun (map (comp println pr-str-instruction decode-instruction) (take 5 (read-frame in)))))
  
  (time (with-open [in (io/input-stream "c:\\Temp\\icfp2006\\sandmark.umz")] 
    (dorun (take-while (complement nil?) (iterate eval-instruction (init-program (read-frame in)))))))
  (with-open [in (io/input-stream "c:\\Temp\\icfp2006\\sandmark.umz")] 
    (let [instructions (vec (read-frame in))
          arr (doto (java.util.HashMap.) (.put 0 (into-array Long/TYPE instructions)))
          reg (long-array 8)]
      (eval-instructions! reg arr 0)))
  (with-open [in (io/input-stream "c:\\Temp\\icfp2006\\sandmark.umz ")] 
    (def p (init-program (read-frame in))))
  )