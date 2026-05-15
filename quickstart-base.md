# Quickstart: Using Statistical Packages

A strong R community has contributed over 20,000 packages to CRAN,
R's central package registry. The F# R Type Provider enables you to
use every single one of them from within the F# environment.

Using RRrovider, you can orchestrate R workflows and manipulate R data,
pass in F# values, and extract R values back to F#.

For this example, we simply demonstrate some basic RProvider concepts
using the built-in `stats` package.

## Example: Linear Regression

Let's perform a simple linear regression from the F# interactive,
using the R.lm function.

Once you have referenced RProvider's nuget package in your script,
library, or app, you can reference the required libraries and packages this way:

```fsharp
open RProvider
open RProvider.Operators

open RProvider.graphics
open RProvider.stats
```

Once the libraries and packages have been loaded,
Imagine that our true model is

Y = 5.0 + 3.0 * X1 - 2.0 * X2 + noise

Let's generate a fake dataset using F# that follows this model:

```fsharp
// Random number generator
let rng = System.Random()
let rand () = rng.NextDouble()

// Generate fake X1 and X2 
let X1s = [ for i in 0 .. 9 -> 10. * rand () ]
let X2s = [ for i in 0 .. 9 -> 5. * rand () ]

// Build Ys, following the "true" model
let Ys = [ for i in 0 .. 9 -> 5. + 3. * X1s.[i] - 2. * X2s.[i] + rand () ]
```

Using linear regression on this dataset, we should be able to
estimate the coefficients 5.0, 3.0 and -2.0, with some imprecision
due to the "noise" part.

Let's first put our dataset into a R dataframe; this allows us
to name our vectors, and use these names in R formulas afterwards:

```fsharp
let dataset = [ 
    "Y" => Ys
    "X1" => X1s
    "X2" => X2s ] |> R.data_frame
```

We can now use R to perform a linear regression.
We call the [R.lm function](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html),
passing it the formula we want to estimate.
(See the [R manual on formulas](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/formula.html)
for more on their somewhat esoteric construction)

```fsharp
let result = R.lm(formula = "Y~X1+X2", data = dataset)
```

## Extracting Results from R to F#

The result we get back from R is a R Expression.
The R Type Provider tries as much as possible to keep data
as R Expressions, rather than converting back-and-forth
between F# and R types. It limits translations
between the 2 languages, which has performance benefits,
and simplifies composing R operations. On the other hand,
we need to extract the results from the R expression
into F# types.

The [R docs for lm](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html)
describes what R.lm returns: a R List. We can now retrieve each element,
accessing it by name (as defined in the documentation).
For instance, let's retrieve the coefficients and residuals,
which are both R vectors containg floats:

```fsharp
let coefficients = result?coefficients.AsVector().AsReal()
let residuals = result?residuals.AsVector().AsReal()
```

We can also produce summary statistics about our model,
like R^2, which measures goodness-of-fit - close to 0
indicates a very poor fit, and close to 1 a good fit.
See [R docs for the details on Summary](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/summary.lm.html).

```fsharp
let summary = R.summary result

summary?``r.squared``.AsScalar()
```

```
NumericS { Sexp = { ptr = 6246263048n } }
```

Finally, we can directly pass results, which is a R expression,
to R.plot, to produce some fancy charts describing our model:

```fsharp
Graphics.svg 8 4 (fun _ -> R.plot result)
```

<?xml version='1.0' encoding='UTF-8' ?>
<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='576.00pt' height='288.00pt' viewBox='0 0 576.00 288.00'>
<g class='svglite'>
<defs>
  <style type='text/css'><![CDATA[
    .svglite line, .svglite polyline, .svglite polygon, .svglite path, .svglite rect, .svglite circle {
      fill: none;
      stroke: #000000;
      stroke-linecap: round;
      stroke-linejoin: round;
      stroke-miterlimit: 10.00;
    }
    .svglite text {
      white-space: pre;
    }
    .svglite g.glyphgroup path {
      fill: inherit;
      stroke: none;
    }
  ]]></style>
</defs>
<rect width='100%' height='100%' style='stroke: none; fill: #FFFFFF;'/>
<defs>
  <clipPath id='cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA='>
    <rect x='0.00' y='0.00' width='576.00' height='288.00' />
  </clipPath>
</defs>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
</g>
<defs>
  <clipPath id='cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng=='>
    <rect x='59.04' y='59.04' width='486.72' height='155.52' />
  </clipPath>
</defs>
<g clip-path='url(#cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng==)'>
</g>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
<line x1='77.07' y1='214.56' x2='494.48' y2='214.56' style='stroke-width: 0.75;' />
<line x1='77.07' y1='214.56' x2='77.07' y2='221.76' style='stroke-width: 0.75;' />
<line x1='181.42' y1='214.56' x2='181.42' y2='221.76' style='stroke-width: 0.75;' />
<line x1='285.77' y1='214.56' x2='285.77' y2='221.76' style='stroke-width: 0.75;' />
<line x1='390.13' y1='214.56' x2='390.13' y2='221.76' style='stroke-width: 0.75;' />
<line x1='494.48' y1='214.56' x2='494.48' y2='221.76' style='stroke-width: 0.75;' />
<text x='77.07' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.0</text>
<text x='181.42' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.1</text>
<text x='285.77' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.2</text>
<text x='390.13' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.3</text>
<text x='494.48' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.4</text>
<line x1='59.04' y1='198.28' x2='59.04' y2='86.29' style='stroke-width: 0.75;' />
<line x1='59.04' y1='198.28' x2='51.84' y2='198.28' style='stroke-width: 0.75;' />
<line x1='59.04' y1='160.95' x2='51.84' y2='160.95' style='stroke-width: 0.75;' />
<line x1='59.04' y1='123.62' x2='51.84' y2='123.62' style='stroke-width: 0.75;' />
<line x1='59.04' y1='86.29' x2='51.84' y2='86.29' style='stroke-width: 0.75;' />
<text transform='translate(41.76,198.28) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='10.67px' lengthAdjust='spacingAndGlyphs'>-2</text>
<text transform='translate(41.76,160.95) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='10.67px' lengthAdjust='spacingAndGlyphs'>-1</text>
<text transform='translate(41.76,123.62) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='6.67px' lengthAdjust='spacingAndGlyphs'>0</text>
<text transform='translate(41.76,86.29) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='6.67px' lengthAdjust='spacingAndGlyphs'>1</text>
<polygon points='59.04,214.56 545.76,214.56 545.76,59.04 59.04,59.04 ' style='stroke-width: 0.75;' />
<text x='302.40' y='269.28' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='50.03px' lengthAdjust='spacingAndGlyphs'>Leverage</text>
<text transform='translate(12.96,136.80) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='122.06px' lengthAdjust='spacingAndGlyphs'>Standardized residuals</text>
</g>
<g clip-path='url(#cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng==)'>
<circle cx='481.27' cy='96.54' r='2.70' style='stroke-width: 0.75;' />
<circle cx='379.79' cy='198.87' r='2.70' style='stroke-width: 0.75;' />
<circle cx='217.92' cy='74.73' r='2.70' style='stroke-width: 0.75;' />
<circle cx='448.00' cy='99.64' r='2.70' style='stroke-width: 0.75;' />
<circle cx='523.50' cy='150.40' r='2.70' style='stroke-width: 0.75;' />
<circle cx='329.94' cy='114.98' r='2.70' style='stroke-width: 0.75;' />
<circle cx='527.73' cy='114.67' r='2.70' style='stroke-width: 0.75;' />
<circle cx='298.38' cy='119.81' r='2.70' style='stroke-width: 0.75;' />
<circle cx='218.69' cy='99.27' r='2.70' style='stroke-width: 0.75;' />
<circle cx='476.05' cy='177.76' r='2.70' style='stroke-width: 0.75;' />
<polyline points='217.92,85.18 218.69,85.50 298.38,120.41 329.94,136.21 379.79,141.52 448.00,113.07 476.05,118.12 481.27,119.84 523.50,132.14 527.73,133.28 ' style='stroke-width: 0.75; stroke: #DF536B;' />
<line x1='59.04' y1='123.62' x2='545.76' y2='123.62' style='stroke-width: 0.75; stroke: #BEBEBE; stroke-dasharray: 1.00,3.00;' />
<line x1='77.07' y1='214.56' x2='77.07' y2='59.04' style='stroke-width: 0.75; stroke: #BEBEBE; stroke-dasharray: 1.00,3.00;' />
</g>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
<text x='302.40' y='283.68' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='403.58px' lengthAdjust='spacingAndGlyphs'>(function (formula, data, subset, weights, na.action, method = "qr", model  ...</text>
</g>
<g clip-path='url(#cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng==)'>
<polyline points='81.57,-570.57 86.22,-362.51 90.86,-271.44 95.50,-217.33 100.14,-180.42 104.78,-153.16 109.42,-131.95 114.07,-114.83 118.71,-100.63 123.35,-88.60 127.99,-78.22 132.63,-69.15 137.28,-61.14 141.92,-53.98 146.56,-47.54 151.20,-41.70 155.84,-36.37 160.49,-31.48 165.13,-26.97 169.77,-22.80 174.41,-18.91 179.05,-15.29 183.69,-11.90 188.34,-8.71 192.98,-5.71 197.62,-2.88 202.26,-0.20 206.90,2.34 211.55,4.76 216.19,7.05 220.83,9.25 225.47,11.34 230.11,13.34 234.75,15.26 239.40,17.10 244.04,18.87 248.68,20.57 253.32,22.21 257.96,23.79 262.61,25.31 267.25,26.78 271.89,28.20 276.53,29.57 281.17,30.91 285.82,32.20 290.46,33.45 295.10,34.66 299.74,35.84 304.38,36.99 309.02,38.11 313.67,39.19 318.31,40.25 322.95,41.28 327.59,42.28 332.23,43.26 336.88,44.22 341.52,45.15 346.16,46.06 350.80,46.95 355.44,47.83 360.09,48.68 364.73,49.51 369.37,50.33 374.01,51.13 378.65,51.91 383.29,52.68 387.94,53.44 392.58,54.17 397.22,54.90 401.86,55.61 406.50,56.31 411.15,57.00 415.79,57.67 420.43,58.34 425.07,58.99 429.71,59.63 434.36,60.26 439.00,60.88 443.64,61.49 448.28,62.09 452.92,62.69 457.56,63.27 462.21,63.85 466.85,64.41 471.49,64.97 476.13,65.52 480.77,66.07 485.42,66.60 490.06,67.13 494.70,67.65 499.34,68.17 503.98,68.68 508.63,69.18 513.27,69.68 517.91,70.17 522.55,70.65 527.19,71.13 531.83,71.60 536.48,72.07 541.12,72.53 545.76,72.99 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,817.81 86.22,609.76 90.86,518.69 95.50,464.57 100.14,427.66 104.78,400.40 109.42,379.20 114.07,362.08 118.71,347.88 123.35,335.84 127.99,325.47 132.63,316.40 137.28,308.38 141.92,301.23 146.56,294.79 151.20,288.95 155.84,283.62 160.49,278.73 165.13,274.22 169.77,270.04 174.41,266.16 179.05,262.54 183.69,259.14 188.34,255.96 192.98,252.96 197.62,250.12 202.26,247.45 206.90,244.90 211.55,242.49 216.19,240.19 220.83,238.00 225.47,235.90 230.11,233.90 234.75,231.98 239.40,230.14 244.04,228.37 248.68,226.67 253.32,225.04 257.96,223.46 262.61,221.94 267.25,220.47 271.89,219.05 276.53,217.67 281.17,216.34 285.82,215.05 290.46,213.80 295.10,212.58 299.74,211.40 304.38,210.25 309.02,209.14 313.67,208.05 318.31,207.00 322.95,205.97 327.59,204.96 332.23,203.98 336.88,203.03 341.52,202.09 346.16,201.18 350.80,200.29 355.44,199.42 360.09,198.57 364.73,197.73 369.37,196.92 374.01,196.12 378.65,195.33 383.29,194.56 387.94,193.81 392.58,193.07 397.22,192.34 401.86,191.63 406.50,190.93 411.15,190.25 415.79,189.57 420.43,188.91 425.07,188.26 429.71,187.61 434.36,186.98 439.00,186.36 443.64,185.75 448.28,185.15 452.92,184.56 457.56,183.97 462.21,183.40 466.85,182.83 471.49,182.27 476.13,181.72 480.77,181.18 485.42,180.64 490.06,180.11 494.70,179.59 499.34,179.08 503.98,178.57 508.63,178.07 513.27,177.57 517.91,177.08 522.55,176.59 527.19,176.12 531.83,175.64 536.48,175.17 541.12,174.71 545.76,174.25 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,-858.11 86.22,-563.88 90.86,-435.08 95.50,-358.55 100.14,-306.35 104.78,-267.80 109.42,-237.81 114.07,-213.61 118.71,-193.52 123.35,-176.50 127.99,-161.83 132.63,-149.01 137.28,-137.67 141.92,-127.55 146.56,-118.44 151.20,-110.18 155.84,-102.64 160.49,-95.73 165.13,-89.35 169.77,-83.45 174.41,-77.96 179.05,-72.83 183.69,-68.03 188.34,-63.53 192.98,-59.28 197.62,-55.28 202.26,-51.49 206.90,-47.90 211.55,-44.48 216.19,-41.23 220.83,-38.13 225.47,-35.17 230.11,-32.34 234.75,-29.62 239.40,-27.02 244.04,-24.52 248.68,-22.12 253.32,-19.80 257.96,-17.57 262.61,-15.42 267.25,-13.34 271.89,-11.33 276.53,-9.38 281.17,-7.50 285.82,-5.67 290.46,-3.90 295.10,-2.19 299.74,-0.52 304.38,1.11 309.02,2.68 313.67,4.22 318.31,5.71 322.95,7.17 327.59,8.59 332.23,9.97 336.88,11.33 341.52,12.65 346.16,13.94 350.80,15.20 355.44,16.43 360.09,17.63 364.73,18.81 369.37,19.97 374.01,21.10 378.65,22.21 383.29,23.30 387.94,24.36 392.58,25.41 397.22,26.43 401.86,27.44 406.50,28.43 411.15,29.40 415.79,30.36 420.43,31.29 425.07,32.22 429.71,33.12 434.36,34.02 439.00,34.89 443.64,35.76 448.28,36.61 452.92,37.45 457.56,38.27 462.21,39.09 466.85,39.89 471.49,40.68 476.13,41.46 480.77,42.23 485.42,42.98 490.06,43.73 494.70,44.47 499.34,45.20 503.98,45.92 508.63,46.63 513.27,47.33 517.91,48.02 522.55,48.71 527.19,49.39 531.83,50.05 536.48,50.72 541.12,51.37 545.76,52.02 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,1105.36 86.22,811.12 90.86,682.33 95.50,605.79 100.14,553.60 104.78,515.05 109.42,485.06 114.07,460.85 118.71,440.76 123.35,423.74 127.99,409.07 132.63,396.25 137.28,384.91 141.92,374.79 146.56,365.68 151.20,357.42 155.84,349.89 160.49,342.97 165.13,336.60 169.77,330.69 174.41,325.20 179.05,320.08 183.69,315.28 188.34,310.77 192.98,306.53 197.62,302.52 202.26,298.73 206.90,295.14 211.55,291.73 216.19,288.47 220.83,285.37 225.47,282.41 230.11,279.58 234.75,276.87 239.40,274.26 244.04,271.76 248.68,269.36 253.32,267.04 257.96,264.81 262.61,262.66 267.25,260.58 271.89,258.57 276.53,256.63 281.17,254.74 285.82,252.92 290.46,251.15 295.10,249.43 299.74,247.76 304.38,246.14 309.02,244.56 313.67,243.03 318.31,241.53 322.95,240.07 327.59,238.65 332.23,237.27 336.88,235.92 341.52,234.60 346.16,233.31 350.80,232.05 355.44,230.82 360.09,229.61 364.73,228.43 369.37,227.27 374.01,226.14 378.65,225.03 383.29,223.95 387.94,222.88 392.58,221.84 397.22,220.81 401.86,219.80 406.50,218.81 411.15,217.84 415.79,216.89 420.43,215.95 425.07,215.03 429.71,214.12 434.36,213.23 439.00,212.35 443.64,211.49 448.28,210.64 452.92,209.80 457.56,208.97 462.21,208.16 466.85,207.36 471.49,206.57 476.13,205.79 480.77,205.02 485.42,204.26 490.06,203.51 494.70,202.77 499.34,202.05 503.98,201.33 508.63,200.62 513.27,199.91 517.91,199.22 522.55,198.54 527.19,197.86 531.83,197.19 536.48,196.53 541.12,195.87 545.76,195.23 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<line x1='69.84' y1='206.46' x2='91.44' y2='206.46' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<text x='94.14' y='210.76' style='font-size: 12.00px;fill: #9E9E9E; font-family: "Arial";' textLength='84.33px' lengthAdjust='spacingAndGlyphs'>Cook's distance</text>
</g>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
<line x1='545.76' y1='195.23' x2='545.76' y2='59.04' style='stroke-width: 0.75;' />
<line x1='545.76' y1='195.23' x2='545.76' y2='195.23' style='stroke-width: 0.75;' />
<line x1='545.76' y1='174.25' x2='545.76' y2='174.25' style='stroke-width: 0.75;' />
<line x1='545.76' y1='72.99' x2='545.76' y2='72.99' style='stroke-width: 0.75;' />
<text x='549.36' y='198.45' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>1</text>
<text x='549.36' y='177.48' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='12.50px' lengthAdjust='spacingAndGlyphs'>0.5</text>
<text x='549.36' y='76.21' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='12.50px' lengthAdjust='spacingAndGlyphs'>0.5</text>
<text x='302.40' y='52.56' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='121.39px' lengthAdjust='spacingAndGlyphs'>Residuals vs Leverage</text>
<text x='376.19' y='203.88' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>2</text>
<text x='472.45' y='182.78' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='10.00px' lengthAdjust='spacingAndGlyphs'>10</text>
<text x='519.90' y='155.42' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>5</text>
</g>
</g>
</svg>

That's it - while simple, we hope this example illustrate
how you would go about to use any existing R statistical package.
While the details would differ, the general approach would
remain the same. Happy modelling!
