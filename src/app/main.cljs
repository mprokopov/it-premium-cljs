(ns app.main
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [reagent.core :as r]
   [cljs-http.client :as http]
   [reagent.dom :as rdom]
   [app.benefits :as ben]
   [cljs.core.async :refer [<!]]))

(goog-define VERBOSE false)
(goog-define API-URI "/ru.json")

(defonce arr (r/atom (sorted-map)))
(defonce counter (r/atom 0))
(defonce counter-package (r/atom 0))
(defonce packages (r/atom (sorted-map)))
(defonce step (r/atom 0))
(defonce locale-resources (r/atom {:price {} :package {}}))
;; (defonce email-locale (r/atom {}))
(defonce proposal-locale (r/atom {}))
(defonce package-locale (r/atom {}))
(defonce benefits-locale (r/atom {}))

(defonce customer (r/atom {:name "" :company "" :phone "" :email ""}))

(defn add-service [{:keys [id price count] :as item}]
  (swap! arr assoc id (assoc item :sum (* price count))))

(defn add-package [package]
  (let [id (swap! counter-package inc)]
    (swap! packages assoc id (assoc package :id id))))

(defn set-check [id]
  (let [counter (get-in @arr [id :count])
        v (pos? counter)]
    (swap! arr assoc-in [id :checked] v)))

(defn toggle-check [id]
  (swap! arr update-in [id :checked] not))

(defn inc-counter [id]
  (swap! arr update-in [id :count] inc)
  (when (pos? (get-in @arr [id :count]))
              (set-check id)))

(defn dec-counter [id]
  (when (pos? (get-in @arr [id :count]))
    (swap! arr update-in [id :count] dec)
    (when (zero? (get-in @arr [id :count]))
      (set-check id))))

(defn next-step []
  (swap! step inc))

(defn prev-step []
  (swap! step dec))

(defn proposal-step! []
  (reset! step 1))

(defn change-counter [id v]
  (let [i (int v)]
    (cond
      (pos? i) (swap! arr assoc-in [id :count] i)
      :else (swap! arr assoc-in [id :count] 0))))

(defn checked-items
  "returns values of items with selected mark"
  []
  (filter :checked (vals @arr)))

(defn calc-price [items]
  (let [calc-fn (fn [item]
                  (* (:price item)
                     (:count item)))]
    (->>
     (map calc-fn items)
     (reduce +))))

(defn price-with-multiplier
  "m map with keys value and multiplier"
  [{basis :basis multiplier :multiplier}]
  (Math/ceil
   (+ basis
      (* multiplier
         (calc-price (checked-items))))))

(defn export []
  (let [export-keys #(select-keys % [:name :price :sum :count])
        assoc-price (fn [val] (assoc val :value (price-with-multiplier val)))]
    (http/post "https://uhlh3yezw6.execute-api.eu-central-1.amazonaws.com/generate-pdf"
               { :with-credentials? false
                 :json-params {
                               :propositions (map export-keys (checked-items))
                               :packages (map #(-> % val assoc-price) @packages)
                               :customer @customer
                               :email (:email @locale-resources)}})
    (when VERBOSE
      (js/console.log
       (clj->js { :propositions (map export-keys (checked-items))
                 :packages (map #(-> % val assoc-price) @packages)
                 :customer @customer })))))

(defn service-amount [{:keys [count id input-name]}]
  [:div.amount-line
   [:div.amount
    [:input.amount-input {:type :text :value count :on-change (fn [e] (change-counter id (-> e .-target .-value)) )}]
    [:div.amount-change
     [:button.up {:on-click #(inc-counter id)} "+"]
     [:button.down {:on-click #(dec-counter id)} "-"]]]
   (when input-name
     [:span.text input-name])])

(defn service [item]
  (let [{:keys [name count checked id has-input input-name] :as m} (val item)]
    [:li
     [:div.checkbox
      [:input {:type :checkbox :id (str "propos_" id) :checked checked :on-change #(toggle-check id)}]
      [:label {:for (str "propos_" id)} name]]
     (when has-input [service-amount m])]))

(defn columns-single [body]
  [:div.columns>div.column body])

(defn price-block [body]
  (columns-single
   [:div.content
    [:div.price-block
    (columns-single
      body)]]))

(defn prices []
  (let [title (get-in @locale-resources [:price :title])]
    [:div.columns
     [:div.column]
     [:div.column.is-four-fifths
      [:div.title.has-text-centered.is-uppercase title]
      [:div.content
       [columns-single
        [:div.price-block
         [:div.columns
          [:div.column
           [:div.content
            [:ul.option-list
             (for [srv (take 4 @arr)]
               ^{:key (str "service-" (key srv))} [service srv])
             ]]]
          [:div.column
           [:div.content
            [:ul.option-list
             (for [srv (drop 4 @arr)]
               ^{:key (str "service-" (key srv))} [service srv])
             ]]]]]]]]
     [:div.column]]))

(defn package-feature [item]
  [:li item])

(defn package-item [{:keys [name price rate features id title label] :as m}]
  [:div.package.column.has-text-centered {:class (str "package--" name)}
   [:div.package__header.is-uppercase (or title name)]
   [:div.package__price
    [:strong (price-with-multiplier m)] rate]
   [:div.package__body
    [:ul.package-list
     (for [feature features]
       ^{:key (str "feature-" id feature)} [package-feature feature])]]])

(defn level [left right]
  [:div.level
   [:div.level-left left]
   [:div.level-right right]
   ])

(defn packages-app []
  [:section.section.prices
   [:div.container
    [:div.title.has-text-centered.is-uppercase (:title @package-locale)]
    [:div.columns.packages__body
     (for [package (map val @packages)]
       ^{:key (str "package-" (:id package))}
       [package-item package])
     ]
    [level
     [:a.level-item.button.is-secondary.is-medium {:on-click prev-step} (:back @package-locale)]
     [:a.level-item.button.is-primary.is-medium {:on-click next-step} (:email @package-locale)]]
    [ben/benefits @benefits-locale]
    ]]
  )

(defn three-columns [body]
  [:div.columns
   [:div.column]
   [:div.column.is-four-fifths body]
   [:div.column]
   ])



(defn validate-email [v]
  (not (empty?
        (re-find #"^.+@.+\..{2,}" v))))

(defn validate-empty
  "returns true if v length is zero"
  [v]
  (pos? (count v)))

(defn field-icon [icon]
  [:span.icon.is-small.is-left
   [:i.fas {:class (str "fa-" icon)}]])

(defonce valid-fields (r/atom {:name false :email false}))

(defn proposal-field []
  (let [dirty? (r/atom false)
        dirty! #(reset! dirty? true)]
    (fn [label k validation-fn icon]
      (let [v (@customer k)
            validate (fn [value]
                       (cond
                         (nil? validation-fn) true ; valid if validation is undefined
                         (false? @dirty?) true ; valid if not dirty yet
                         @dirty? (validation-fn value)))]
        [:div.field
         [:label.label label #_(when validation-fn "*")]
         [:div.control {:class (when icon "has-icons-left")}
          [:input.input {:class (when-not (validate v) "is-danger")
                         :type "text"
                         :value v
                         :on-change (fn [e]
                                      (do (dirty!)
                                          (let [value (.. e -target -value)]
                                            (when validation-fn
                                              (swap! valid-fields assoc k (validate value)))
                                            (swap! customer assoc k value))))}]
          (when icon [field-icon icon])]]))))

(defn send-handler [e]
  (export)
  (next-step)
  (.preventDefault e)
  )

(defn proposal []
  (let [valid? (r/atom true)]
    (fn []
      [:div.modal-content.proposal
       [:div.proposal__title.has-text-centered.title.is-uppercase
        [:h2 (:title @proposal-locale)]]
       [:div.columns.is-variable.is-6
        [:div.column.is-half]
        [:div.column.proposal__form
         [:form {:on-submit #(false)}
          [proposal-field (:name @proposal-locale) :name validate-empty]
          [proposal-field (:phone @proposal-locale) :phone]
          [proposal-field (:company @proposal-locale) :company]
          [proposal-field (:email @proposal-locale) :email validate-email "envelope"]
          [:div.field>div.control>button.button.is-primary {:on-click send-handler :disabled (not (every? true? (vals @valid-fields)))} (:button @proposal-locale)]
          ]]]])))

(defn thanks-modal []
  [:div.modal.is-active
   [:div.modal-background {:on-click prev-step}]
   [proposal]
   [:button.modal-close.is-large {:aria-label "close" :on-click prev-step}]
   ])

(defn mail-sent[]
  (let [email (:email @customer)]
    [:div.modal-content.thanks
     [:div.thanks__title.has-text-centered.title.is-uppercase (get-in @locale-resources [:proposal :thank])]
     [:div.has-text-centered
      [:p.thanks__body (:checkmail @proposal-locale) [:strong " " email]]
       [:button.button.is-primary {:on-click proposal-step!} "Ok"]
       ]]))

(defn thanks-sent []
  [:div.modal.is-active
   [:div.modal-background {:on-click proposal-step!}]
   [mail-sent]
   [:button.modal-close.is-large {:aria-label "close" :on-click proposal-step!}]
   ])

(defn prices-app []
  [:section.section.prices
   [:div.container
    [prices]
    [three-columns
     [level ""
      [:button.level-item.button.is-primary.is-medium {:on-click next-step} (get-in @locale-resources [:price :calculate])]]]
    [ben/benefits @benefits-locale]]])

(defn app []
  (let [step1 (r/atom 0)]
    (case @step
      0 [prices-app]
      1 [packages-app]
      2 [thanks-modal]
      3 [thanks-sent])))

(defn opt-in-email []
  [:input.input.subscribe__input
   {:type :text
    :on-change #(swap! customer assoc :email (.. % -target -value))
    :value (:email @customer)}])

(defn reload! []
  (println "Code updated.")
  (rdom/render [opt-in-email]
               (.getElementById js/document "optin_email"))
  (rdom/render [app]
            (.getElementById js/document "app")))

(defn main! []
  ;; (js/console.log "Starting...")

  (rdom/render [opt-in-email]
               (.getElementById js/document "optin_email"))
  (rdom/render [app]
            (.getElementById js/document "app")))

(defn load-data []
  (go (let [response (<! (http/get "index.json" {:with-credentials? false}))]
        (reset! packages (sorted-map))
        (reset! arr (sorted-map))
        (let [body (js->clj (:body response))
              packages (:packages body)
              services (:services body)]
          (doseq [service services]
            (let [transformed (assoc service
                                     :count (get service :default_selected 1)
                                     :checked (not(nil? (:default_selected service)))
                                     :input-name (:input_name service)
                                     :has-input (:has_input service))]
              (add-service transformed)))
          (do
            (let [{:keys [price-locale email package-locale proposal]}body]
              (swap! locale-resources assoc :price price-locale
                                            :email email
                                            :proposal proposal
                                            :package-locale package-locale))
            ;; (reset! email-locale (:email body))
            (reset! package-locale (:package-locale body))
            (reset! benefits-locale (:benefits body))
            (reset! proposal-locale (:proposal body)))
          (doseq [package packages]
            (let [transformed (assoc package :name (:id package))]
              (add-package transformed)))
          (prn "Data loaded.")))))

(defonce init (load-data))
