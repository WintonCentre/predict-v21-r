# R model for Predict V2.1

This library contains the R model and a clojurescript adapter which
allows a node application running clojurescript to shell out to the R model.

If you are wanting to use the code to validate the PREDICT algorithm in a new population, please contact us at predict@statslab.cam.ac.uk and we'd be delighted to help. The new version, v3, is about to be released and it would be better to use that version.

## Installation

```
lein install
```

In your leiningen `project.clj` add  \[predict-r-model "0.1.0"\] to your dependencies.

Then:

```
(require '[predict.models.r.predict :refer [r-predict]]
```
