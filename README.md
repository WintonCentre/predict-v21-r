# R model for Predict V2.1

This library contains the R model and a clojurescript adapter which
allows a node application running clojurescript to shell out to the R model.

## Installation

```
lein install
```

In your leiningen `project.clj` add  \[predict-r-model "0.1.0"\] to your dependencies.

Then:

```
(require '[predict.models.r.predict :refer [r-predict]]
```