# survSAKK 1.2.1

This update brings a bug fix and small adaptions. 

🐛  **Bug Fix**
  
Correction of number of censored patients in risk table at time 0.

🛠️ **Improvement of github page**
  
- Adaption in citation
- Additional example in Vignette how to export figures for reports and posters

--------------------------------------------------------------------------------

# survSAKK 1.2.0

This update brings new features

✨  **New Feature**

- `risktable.censoring()` A logical parameter indicating whether to display number of censored patients. Default is set to `FALSE`.

- `theme()` Built-in layout options for various congress.


🛠️ **Improvement of the segment***

Function can handle annotation of  multiple time points and quantiles. Note: segment.main` is not supported if more than one time point quantiles are given.

🎨 **Adjustments**

`stat="logrank"` will write just write ‘p=...’ and not ‘Logrank test: p=...’

--------------------------------------------------------------------------------

# survSAKK 1.1.0

This update brings significant improvements and a new feature based on your feedback from initial release. 

✨  **New Feature**

- `segment.annotation.two.lines()` A logical parameter to force that the annotation is displayed on two lines even if there is only one arm. This parameter only has an effect if there is only one arm (Default: `FALSE`).

🐛  **Bug Fixes**

- NA to NR Replacement

- Special Character Support
  
  Fixed  issues to allow special character strings ≥ and ≤ in label names.
  
- Rounding

  Improved handling and rounding, espcially when dealing with special characters.

--------------------------------------------------------------------------------

# survSAKK 1.0.0

🎉 First Release!

--------------------------------------------------------------------------------

# survSAKK 0.0.0.900

🚩 Initial GitHub Submission.
