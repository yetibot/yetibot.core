(ns yetibot.core.commands.resolve
  (:require
   [yetibot.core.hooks :refer [cmd-hook]]))

(defn resolve-cmd
  "resolve <key> # resolve <key> at the narrowest scope"
  [{:keys [match]}]
  match)

(cmd-hook
 #"resolve"
 #"(\S.+)" resolve-cmd)
