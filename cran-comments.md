## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


Additional response:

```
Please proof-read your description text.
Currently it reads: " ... tools to exports the data into common ..."
But probably it should be: " ... tools to export the data into common ..."
```

Thanks, fixed.

```
If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")
```

Added `https` link to `Blackrock` website. There is no reference to any paper. 

The users of this package are quite targeted (and also very limited unfortunately): they will be most likely to be neuroscientists who perform human/monkey/mice intracranial electroencephalography (iEEG) study. They will know what's `Blackrock-Microsystems` and what are `NEV`, `NSx` files. Other people are less likely to get access to the these files. This is because there was no packages in R that can read those files. Therefore the raw files are often translated into other files before handed over to statisticians. The scope of this package is very similar to an existing CRAN package (`edfReader`).

Also just in case. The license of this package has been confirmed with `Blackrock` with email evidences. The condition is I need release `readNSx` with open-sourced license and it has to be free of charge.

```

The LICENSE file is only needed if you have additional restrictions to the license which you have not? In that case omit the file and its reference in the DESCRIPTION file.

Please fix and resubmit.

Best,
Victoria Wimmer
```

Yes, the LICENSE file DOES provide additional copyright notice that is allowed by "MPL-2.0: Exhibit A - Source Code Form License Notice" as cited below:

```
You may add additional accurate notices of copyright ownership.
```

Please see Line 369-373 for additional copyright ownership:

```
YEAR: 2023
COPYRIGHT HOLDER: Zhengjia Wang
```
