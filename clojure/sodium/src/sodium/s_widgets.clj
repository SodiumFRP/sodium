(ns sodium.s-widgets
  (:require [sodium.core :refer :all])
  (:require [clojure.string :as s])
  (:import [nz.sodium Cell  CellLoop Listener Stream StreamLoop StreamSink Operational Transaction Unit])
  (:import [javax.swing JButton JComponent JFrame JLabel JPanel JTextField SwingUtilities])
  (:import [java.awt FlowLayout GridBagLayout GridBagConstraints])
  (:import [java.awt.event ActionListener])
  (:import [javax.swing.event DocumentEvent DocumentListener])
  (:gen-class))

(defn update-text [source sink]
  (let [text (.getText source)]
    (SwingUtilities/invokeLater (fn []
                                  (if (not= text nil)
                                    (do
                                      (.send sink text)))))))
                                          
(defn listen [source sink]
  (let [dl (proxy [DocumentListener] []
             (insertUpdate[e] (update-text source sink))
             (removeUpdate [e] (update-text source sink))
             (changedUpdate [e] (update-text source sink)))]
    (.. source getDocument
        (addDocumentListener dl))
    dl))

(defn s-button
  ([label] (s-button label (Cell. true)))
  ([label enabled]
     (let [s-clicked-sink (StreamSink.)
           l (atom 0)
           button (proxy [JButton] [label]
                    (removeNotify []
                      (.unlisten @l)
                      (proxy-super removeNotify)))]
       (.addActionListener button
                           (proxy [ActionListener] []
                             (actionPerformed [event] (.send s-clicked-sink Unit/UNIT))))
       (Transaction/post (fn []
                           (.setEnabled button (.sample enabled))))
       (reset! l (.listen (Operational/updates enabled) (handler [ena]
                                                                 (if (SwingUtilities/isEventDispatchThread)
                                                                   (.setEnabled button ena)
                                                                   (SwingUtilities/invokeLater (fn []
                                                                                                 (.setEnabled button ena)))))))
       {:jbutton button :s-clicked s-clicked-sink :listner l})))
     
         

(defn s-label
  [c-text]
  (let [l (atom 0)
        label (proxy [JLabel] [""]
                (removeNotify []
                  (.unlisten @l)
                  (proxy-super removeNotify)))]
    (reset! l (.listen (Operational/updates c-text)
                       (handler [t]
                                (if (SwingUtilities/isEventDispatchThread)
                                  (.setText label t)
                                  (SwingUtilities/invokeLater (fn [t] (.setText label ) t))))))
    (Transaction/post (fn [] (SwingUtilities/invokeLater (fn []
                                                          (.setText label (.sample c-text))))))
    label))

(defn s-text-field
  ([init-text] (s-text-field (Stream.) init-text 15))
  ([string-or-stream string-or-width]
     (if (string? string-or-stream)
       (s-text-field (Stream.) string-or-stream string-or-width (Cell. true))
       (s-text-field string-or-stream string-or-width 15 (Cell. true))))
  ([string-or-stream string-or-width cell-or-width]
     (if (string? string-or-stream)
       (s-text-field (Stream.) string-or-stream string-or-width cell-or-width)
       (s-text-field string-or-stream string-or-width cell-or-width (Cell. true))))
  ;(s-text-field s-text init-text width (Cell. true)))
  ([^Stream s-text ^String init-text ^long width ^Cell enabled]
   (let [s-decrement (StreamSink.)
         allow (.map
                (.accum
                 (.orElse
                  (.map s-text (apply1 [u] 1))
                  s-decrement)
                 0
                 (apply2 [d b] (+ b d)))
                (apply1 [b] (== b 0)))
         l (atom 0)
         s-user-changes (StreamSink.)
         text (.hold (.orElse (.gate s-user-changes allow) s-text) init-text)
         s-text-field (proxy [JTextField] [init-text width]
                        (removeNotify []
                          (.unlisten @l)
                          (proxy-super removeNotify)))
         dl (listen s-text-field s-user-changes)]
     (Transaction/post (fn [] (.setEnabled s-text-field (.sample enabled))))
     (reset! l (.append (.listen s-text (handler [text] (SwingUtilities/invokeLater
                                                         (fn []
                                                           (.. s-text-field getDocument
                                                               (removeDocumentListener dl))
                                                           (.setText s-text-field text)
                                                           (.. s-text-field getDocument
                                                               (addDocumentListener dl))
                                                           (.send s-decrement -1)))))
                        (.listen (Operational/updates enabled)
                                 (handler [ena] (if (SwingUtilities/isEventDispatchThread)
                                                  (do
                                                    (.setEnabled s-text-field ena))
                                                  (do
                                                    (SwingUtilities/invokeLater (fn []
                                                                                (.setEnabled s-text-field ena)))))))))
     {:jcomp s-text-field :jtext s-text-field :cell text :stream s-user-changes :dl dl})))
  
(defn label
  []
 (let [frame (JFrame. "label")
       msg (s-text-field "Hello")
       label (s-label (:cell msg))]
   (doto frame
     (.setLayout (FlowLayout.))
     (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
     (.add (:jtext msg))
     (.add label)
     (.setSize 400 160)
     (.setVisible true))))

(defn frp-reverse
  []
 (let [frame (JFrame. "reverse")
       msg (s-text-field "Hello")
       reversed (.map (:cell msg) (apply1 [t]
                                          (s/reverse t)))
       label (s-label reversed)]
   (doto frame
     (.setLayout (FlowLayout.))
     (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
     (.add (:jtext msg))
     (.add label)
     (.setSize 400 160)
     (.setVisible true))))

(defn gamechat
  []
  (let [frame (JFrame. "gamechat")
        onegai (s-button "Onegai shimasu" (Cell. true))
        thanks (s-button "Thank you" (Cell. true))
        s-onegai (.map (:s-clicked onegai) (apply1 [u](str "Onegai shimasu")))
        s-thanks (.map (:s-clicked thanks) (apply1 [u] (str "Thank you")))
        s-canned (.orElse s-onegai s-thanks)
        text (s-text-field s-canned "")]
    (doto frame
      (.setLayout (FlowLayout.))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (:jtext text))
      (.add (:jbutton onegai))
      (.add (:jbutton thanks))
      (.setSize 400 160)
      (.setVisible true))))

(defn redgreen
  []
  (let [frame (JFrame. "redgreen")
        red (s-button "red")
        green (s-button "green")
        s-red (.map (:s-clicked red) (apply1 [u] (str "red")))
        s-green (.map (:s-clicked green) (apply1 [u] (str "green")))
        s-color (.orElse s-red s-green)
        color (.hold s-color "")
        lbl (s-label color)]
    (doto frame
      (.setLayout (FlowLayout.))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (:jbutton red))
      (.add (:jbutton green))
      (.add lbl)
      (.setSize 400 160)
      (.setVisible true))))

(defn translate
  []
  (let [frame (JFrame. "translate")
        english (s-text-field "I like FRP")
        translate (s-button "Translate")
        s-latin (.snapshot (:s-clicked translate) (:cell english) (apply2 [u txt]
                                                                          (.trim (.. txt trim
                                                                                     (replaceAll " |$" "us ")))))
        latin (.hold s-latin "")
        lbl-latin (s-label latin)]
    (doto frame
      (.setLayout (FlowLayout.))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (:jtext english))
      (.add (:jbutton translate))
      (.add lbl-latin)
      (.setSize 400 160)
      (.setVisible true))))

(defn spinner
  []
  (let [view (JFrame. "spinner")]
    (Transaction/runVoid (fn []
                           (let [value (CellLoop.)
                               lbl-value (s-label (.map value (apply1 [i] (str i))))
                               plus (s-button "+")
                               minus (s-button "-")
                                 s-plus-delta (.map (:s-clicked plus) (apply1 [u]
                                                                              1))
                               s-minus-delta (.map (:s-clicked minus) (apply1 [u] -1))
                               s-delta (.orElse s-plus-delta s-minus-delta)
                               s-update (.snapshot s-delta value (apply2 [delta value_] (+ delta value_)))]
                             (.loop value (.hold s-update 0))
                             (doto view
                               (.setLayout (FlowLayout.))
                               (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                               (.add lbl-value)
                               (.add (:jbutton plus))
                               (.add (:jbutton minus))))))
  (doto view
    (.setSize 400 160)
    (.setVisible true))))

(defn add
  []
  (let [frame (JFrame. "add")
        txt-a (s-text-field "5")
        txt-b (s-text-field "10")
        parse-int (fn [t] (try
                           (Integer/parseInt t)
                           (catch NumberFormatException e
                             0)))
        a (.map (:cell txt-a) (apply1 [t] (parse-int t)))
        b (.map (:cell txt-b) (apply1 [t] (parse-int t)))
        sum (Cell/lift (apply2 [a_ b_] (+ a_ b_)) a b)
        lbl-sum (s-label (.map sum (apply1 [i] (str i))))]
    (doto frame
      (.setLayout (FlowLayout.))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (:jtext txt-a))
      (.add (:jtext txt-b))
      (.add lbl-sum)
      (.setSize 400 160)
      (.setVisible true))))

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))

(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

(defn s-spinner
  [initial-value]
  (let [s-set-value (StreamLoop.)
        text-field (s-text-field (.map s-set-value (apply1 [v] (str v))) (str initial-value) 5)
        parse-int (fn [t] (try
                           (Integer/parseInt t)
                           (catch NumberFormatException e
                             0)))
        value (.map (:cell text-field) (apply1 [txt] (parse-int txt)))
        plus (s-button "+")
        minus (s-button "-")
        spinner (JPanel. (GridBagLayout.))
        s-plus-delta (.map (:s-clicked plus) (apply1 [u] 1))
        s-minus-delta (.map (:s-clicked minus) (apply1 [u] -1))
        s-delta (.orElse s-plus-delta s-minus-delta )]
    (doto spinner
      (grid-bag-layout
       :gridx 0 :gridy 0
       :gridwidth 1 :gridheight 2
       :fill :BOTH
       :weightx 1.0
       (:jtext text-field)
       :gridx 1 :gridy 0
       :gridwidth 1 :gridheight 1
       (:jbutton plus)
       :gridx 1 :gridy 1
       (:jbutton minus)))
    (.loop s-set-value (.snapshot s-delta value (apply2 [delta value]
                                                        (+ delta value))))
    {:jcomp spinner :jpanel spinner :cell value}))

(defn spinme
  []
  (let [view (JFrame.)]
    (Transaction/runVoid (fn []
                           (let [spnr (s-spinner 0)]
                             (.add view (:jpanel spnr)))))
    (doto view
      (.setLayout (FlowLayout.))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize 400 160)
      (.setVisible true))))

(defn form-validation
  []
  (let [view (JFrame. "formvalidation")]
    (Transaction/runVoid
     (fn []
       (let [max-emails 4
             gridbag (GridBagLayout.)
             all-valid (atom (Cell. true))
             valids (atom {})
             x (atom {:row0 { :label (JLabel. "Name")
                             :component (s-text-field "" 30)}
                      :row1 { :label (JLabel. "No of email addresses")
                             :component (s-spinner 1)}})]
         (reset! valids (conj @valids {:valid0
                                       (.map (:cell (:component (:row0 @x)))
                                             (apply1 [t]
                                                     (cond
                                                      (= "" (s/trim t))
                                                      "<-- enter something"
                                                      (< (.indexOf (s/trim t) " ") 0)
                                                      "<-- must contain space"
                                                      :else "")))
                                     :valid1   (.map (:cell (:component (:row1 @x))) (apply1 [n]
                                                                                             (if (or (< n 1)
                                                                                                     (> n max-emails))
                                                         (str "<-- must be 1 to " max-emails)
                                                         "")))
                                     }))
       
         (dotimes [row max-emails]
           (let [email (+ row 1)
                 enabled (.map (:cell (:component (:row1 @x)))
                               (apply1 [n] (< row n)))
                 k-row (keyword (str "row" (+ row 2)))
                 k-valid (keyword (str "valid" (+ row 2)))]
             (reset! x (conj @x {k-row
                                 {:label (JLabel. (str "Email #" email))
                                  :component (s-text-field "" 30 enabled) }}))
             (reset! valids (conj @valids {k-valid
                                           (Cell/lift (apply2 [e n]
                                                              (cond
                                                               (>= row n)
                                                               ""
                                                               (= "" (s/trim e))
                                                               "<-- enter something"
                                                               (< (.indexOf e "@") 0)
                                                               "<-- must contain @"
                                                               :else ""))
                                                     (:cell (:component (k-row @x)))
                                                     (:cell (:component (:row1 @x))))
                                           } ))))
         (doto view
           (.setLayout gridbag)
           (comment grid-bag-layout
            :fill :BOTH
            :gridwidth 1 :gridheight 1
            ))
         (dotimes [row (+ max-emails 2)]
           (let [k-row (keyword (str "row" row))
                 k-valid (keyword (str "valid" row))]
             (doto view
               (grid-bag-layout
                :fill :BOTH
                :gridwidth 1 :gridheight 1
                :weightx 0.0
                :gridx 0 :gridy row
                (:label (k-row @x))
                :weightx 1.0
                :gridx 1
                (:jcomp (:component (k-row @x)))
                :weightx 0.0
                :gridx 2
                (s-label (k-valid @valids))))
             (reset! all-valid (Cell/lift (apply2 [a b]
                                                  (and a b))
                                          @all-valid
                                          (.map (k-valid @valids) (apply1 [t] (= "" t)))))))
         (doto view
           (grid-bag-layout
            :weightx 1.0
            :gridx 0 :gridy 6
            :gridwidth 3
            :fill :NONE
            (:jbutton (s-button "OK" @all-valid)))))))
    (doto view
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize 600 200)
      (.setVisible true))))

                               
(defn -main
  [& args]
  (if (= nil args)
    (do
      (form-validation)
      (spinme)
      (add)
      (spinner)
      (translate)
      (redgreen)
      (gamechat)
      (frp-reverse)
      (label))
    (doall
     (map 
      (fn [s]
        ((ns-resolve 'sodium.s-widgets (symbol s)))) args))))



