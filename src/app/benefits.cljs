(ns app.benefits)

(def items [
            {:title "Тест-драйв на місяць"
             :description "Якщо по завершенню місяця Вам не сподобалися наші послуги, ви їх не оплачуєте."
             :icon "key"
             }
            {:title "Безкоштовний аудит"
             :description "Проведення аудиту Вашої ІТ інфраструктури."
             :icon "battery"
             }
            {:title "Моніторинг обладнання"
             :description "Моніторинг обладнання дозволяє своєчасно сповіщати інженерів про екстрену ситуацію для прийняття заходів щодо усунення проблем."
             :icon "pulse"}
            ])

(defn benefit [{:keys [title description icon]}]
  [:div.column>div.content.benefit
   [:div.benefit__icon
    [:span {:class (str icon "-icon")}]]
   [:div.benefit__description
    [:h5 title]
    [:p.is-small description]
    ]
   ]
  )

(defn benefits [arr]
    [:div.columns.benefits
     (for [item arr]
       ^{:key (:icon item)} [benefit item])])
