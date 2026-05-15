(**

*)
#r "nuget: RProvider,{{package-version}}"
(**
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

*)
open RProvider
open RProvider.Operators

open RProvider.graphics
open RProvider.stats
(**
Once the libraries and packages have been loaded,
Imagine that our true model is

Y = 5.0 + 3.0 * X1 - 2.0 * X2 + noise

Let's generate a fake dataset using F# that follows this model:

*)
// Random number generator
let rng = System.Random()
let rand () = rng.NextDouble()

// Generate fake X1 and X2 
let X1s = [ for i in 0 .. 9 -> 10. * rand () ]
let X2s = [ for i in 0 .. 9 -> 5. * rand () ]

// Build Ys, following the "true" model
let Ys = [ for i in 0 .. 9 -> 5. + 3. * X1s.[i] - 2. * X2s.[i] + rand () ]
(**
Using linear regression on this dataset, we should be able to
estimate the coefficients 5.0, 3.0 and -2.0, with some imprecision
due to the "noise" part.

Let's first put our dataset into a R dataframe; this allows us
to name our vectors, and use these names in R formulas afterwards:

*)
let dataset = [ 
    "Y" => Ys
    "X1" => X1s
    "X2" => X2s ] |> R.data_frame
(**
We can now use R to perform a linear regression.
We call the [R.lm function](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html),
passing it the formula we want to estimate.
(See the [R manual on formulas](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/formula.html)
for more on their somewhat esoteric construction)

*)
let result = R.lm(formula = "Y~X1+X2", data = dataset)
(**
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

*)
let coefficients = result?coefficients.AsVector().AsReal()
let residuals = result?residuals.AsVector().AsReal()
(**
We can also produce summary statistics about our model,
like R^2, which measures goodness-of-fit - close to 0
indicates a very poor fit, and close to 1 a good fit.
See [R docs for the details on Summary](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/summary.lm.html).

*)
let summary = R.summary result

summary?``r.squared``.AsScalar()(* output: 
NumericS { Sexp = { ptr = 6241325512n } }*)
(**
Finally, we can directly pass results, which is a R expression,
to R.plot, to produce some fancy charts describing our model:

*)
Graphics.svg 8 4 (fun _ -> R.plot result)(* output: 
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
<line x1='77.07' y1='214.56' x2='489.13' y2='214.56' style='stroke-width: 0.75;' />
<line x1='77.07' y1='214.56' x2='77.07' y2='221.76' style='stroke-width: 0.75;' />
<line x1='180.08' y1='214.56' x2='180.08' y2='221.76' style='stroke-width: 0.75;' />
<line x1='283.10' y1='214.56' x2='283.10' y2='221.76' style='stroke-width: 0.75;' />
<line x1='386.11' y1='214.56' x2='386.11' y2='221.76' style='stroke-width: 0.75;' />
<line x1='489.13' y1='214.56' x2='489.13' y2='221.76' style='stroke-width: 0.75;' />
<text x='77.07' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.0</text>
<text x='180.08' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.1</text>
<text x='283.10' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.2</text>
<text x='386.11' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.3</text>
<text x='489.13' y='240.48' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='16.67px' lengthAdjust='spacingAndGlyphs'>0.4</text>
<line x1='59.04' y1='190.32' x2='59.04' y2='81.02' style='stroke-width: 0.75;' />
<line x1='59.04' y1='190.32' x2='51.84' y2='190.32' style='stroke-width: 0.75;' />
<line x1='59.04' y1='153.88' x2='51.84' y2='153.88' style='stroke-width: 0.75;' />
<line x1='59.04' y1='117.45' x2='51.84' y2='117.45' style='stroke-width: 0.75;' />
<line x1='59.04' y1='81.02' x2='51.84' y2='81.02' style='stroke-width: 0.75;' />
<text transform='translate(41.76,190.32) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='10.67px' lengthAdjust='spacingAndGlyphs'>-1</text>
<text transform='translate(41.76,153.88) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='6.67px' lengthAdjust='spacingAndGlyphs'>0</text>
<text transform='translate(41.76,117.45) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='6.67px' lengthAdjust='spacingAndGlyphs'>1</text>
<text transform='translate(41.76,81.02) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='6.67px' lengthAdjust='spacingAndGlyphs'>2</text>
<polygon points='59.04,214.56 545.76,214.56 545.76,59.04 59.04,59.04 ' style='stroke-width: 0.75;' />
<text x='302.40' y='269.28' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='50.03px' lengthAdjust='spacingAndGlyphs'>Leverage</text>
<text transform='translate(12.96,136.80) rotate(-90)' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='122.06px' lengthAdjust='spacingAndGlyphs'>Standardized residuals</text>
</g>
<g clip-path='url(#cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng==)'>
<circle cx='345.62' cy='104.47' r='2.70' style='stroke-width: 0.75;' />
<circle cx='338.24' cy='140.72' r='2.70' style='stroke-width: 0.75;' />
<circle cx='481.30' cy='74.73' r='2.70' style='stroke-width: 0.75;' />
<circle cx='441.71' cy='176.98' r='2.70' style='stroke-width: 0.75;' />
<circle cx='361.01' cy='168.62' r='2.70' style='stroke-width: 0.75;' />
<circle cx='401.60' cy='154.99' r='2.70' style='stroke-width: 0.75;' />
<circle cx='238.01' cy='198.87' r='2.70' style='stroke-width: 0.75;' />
<circle cx='379.23' cy='194.44' r='2.70' style='stroke-width: 0.75;' />
<circle cx='527.73' cy='170.63' r='2.70' style='stroke-width: 0.75;' />
<circle cx='346.69' cy='148.72' r='2.70' style='stroke-width: 0.75;' />
<polyline points='238.01,198.62 338.24,127.63 345.62,139.36 346.69,141.06 361.01,164.09 379.23,172.54 401.60,169.86 441.71,173.17 481.30,171.34 527.73,171.57 ' style='stroke-width: 0.75; stroke: #DF536B;' />
<line x1='59.04' y1='153.88' x2='545.76' y2='153.88' style='stroke-width: 0.75; stroke: #BEBEBE; stroke-dasharray: 1.00,3.00;' />
<line x1='77.07' y1='214.56' x2='77.07' y2='59.04' style='stroke-width: 0.75; stroke: #BEBEBE; stroke-dasharray: 1.00,3.00;' />
</g>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
<text x='302.40' y='283.68' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='403.58px' lengthAdjust='spacingAndGlyphs'>(function (formula, data, subset, weights, na.action, method = "qr", model  ...</text>
</g>
<g clip-path='url(#cpNTkuMDR8NTQ1Ljc2fDU5LjA0fDIxNC41Ng==)'>
<polyline points='81.57,-519.25 86.22,-317.49 90.86,-229.18 95.50,-176.69 100.14,-140.90 104.78,-114.46 109.42,-93.89 114.07,-77.29 118.71,-63.52 123.35,-51.84 127.99,-41.78 132.63,-32.98 137.28,-25.21 141.92,-18.27 146.56,-12.02 151.20,-6.35 155.84,-1.18 160.49,3.56 165.13,7.94 169.77,11.99 174.41,15.76 179.05,19.27 183.69,22.57 188.34,25.66 192.98,28.57 197.62,31.32 202.26,33.92 206.90,36.39 211.55,38.73 216.19,40.96 220.83,43.09 225.47,45.12 230.11,47.07 234.75,48.93 239.40,50.72 244.04,52.43 248.68,54.08 253.32,55.67 257.96,57.20 262.61,58.68 267.25,60.11 271.89,61.49 276.53,62.83 281.17,64.12 285.82,65.37 290.46,66.59 295.10,67.77 299.74,68.91 304.38,70.03 309.02,71.11 313.67,72.17 318.31,73.19 322.95,74.19 327.59,75.17 332.23,76.12 336.88,77.05 341.52,77.96 346.16,78.84 350.80,79.71 355.44,80.56 360.09,81.39 364.73,82.20 369.37,82.99 374.01,83.77 378.65,84.53 383.29,85.28 387.94,86.01 392.58,86.73 397.22,87.44 401.86,88.13 406.50,88.81 411.15,89.48 415.79,90.13 420.43,90.78 425.07,91.41 429.71,92.04 434.36,92.65 439.00,93.26 443.64,93.85 448.28,94.44 452.92,95.01 457.56,95.58 462.21,96.14 466.85,96.69 471.49,97.24 476.13,97.77 480.77,98.30 485.42,98.82 490.06,99.34 494.70,99.85 499.34,100.35 503.98,100.84 508.63,101.33 513.27,101.82 517.91,102.29 522.55,102.77 527.19,103.23 531.83,103.69 536.48,104.15 541.12,104.60 545.76,105.05 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,827.02 86.22,625.26 90.86,536.94 95.50,484.46 100.14,448.67 104.78,422.23 109.42,401.66 114.07,385.06 118.71,371.28 123.35,359.61 127.99,349.55 132.63,340.75 137.28,332.98 141.92,326.03 146.56,319.78 151.20,314.12 155.84,308.95 160.49,304.20 165.13,299.83 169.77,295.78 174.41,292.01 179.05,288.49 183.69,285.20 188.34,282.11 192.98,279.20 197.62,276.45 202.26,273.85 206.90,271.38 211.55,269.04 216.19,266.81 220.83,264.68 225.47,262.65 230.11,260.70 234.75,258.84 239.40,257.05 244.04,255.34 248.68,253.69 253.32,252.10 257.96,250.56 262.61,249.09 267.25,247.66 271.89,246.28 276.53,244.94 281.17,243.65 285.82,242.40 290.46,241.18 295.10,240.00 299.74,238.85 304.38,237.74 309.02,236.66 313.67,235.60 318.31,234.57 322.95,233.57 327.59,232.60 332.23,231.65 336.88,230.72 341.52,229.81 346.16,228.92 350.80,228.06 355.44,227.21 360.09,226.38 364.73,225.57 369.37,224.78 374.01,224.00 378.65,223.24 383.29,222.49 387.94,221.76 392.58,221.04 397.22,220.33 401.86,219.64 406.50,218.96 411.15,218.29 415.79,217.63 420.43,216.99 425.07,216.36 429.71,215.73 434.36,215.12 439.00,214.51 443.64,213.92 448.28,213.33 452.92,212.76 457.56,212.19 462.21,211.63 466.85,211.08 471.49,210.53 476.13,210.00 480.77,209.47 485.42,208.94 490.06,208.43 494.70,207.92 499.34,207.42 503.98,206.92 508.63,206.43 513.27,205.95 517.91,205.47 522.55,205.00 527.19,204.54 531.83,204.07 536.48,203.62 541.12,203.17 545.76,202.72 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,-798.07 86.22,-512.75 90.86,-387.85 95.50,-313.62 100.14,-263.00 104.78,-225.61 109.42,-196.53 114.07,-173.05 118.71,-153.57 123.35,-137.06 127.99,-122.82 132.63,-110.39 137.28,-99.39 141.92,-89.57 146.56,-80.73 151.20,-72.72 155.84,-65.41 160.49,-58.70 165.13,-52.51 169.77,-46.78 174.41,-41.45 179.05,-36.48 183.69,-31.83 188.34,-27.45 192.98,-23.34 197.62,-19.45 202.26,-15.77 206.90,-12.28 211.55,-8.97 216.19,-5.81 220.83,-2.80 225.47,0.070 230.11,2.82 234.75,5.45 239.40,7.98 244.04,10.41 248.68,12.74 253.32,14.99 257.96,17.16 262.61,19.25 267.25,21.27 271.89,23.22 276.53,25.11 281.17,26.94 285.82,28.71 290.46,30.43 295.10,32.10 299.74,33.72 304.38,35.29 309.02,36.83 313.67,38.32 318.31,39.77 322.95,41.19 327.59,42.57 332.23,43.91 336.88,45.23 341.52,46.51 346.16,47.76 350.80,48.99 355.44,50.19 360.09,51.36 364.73,52.50 369.37,53.63 374.01,54.73 378.65,55.81 383.29,56.86 387.94,57.90 392.58,58.91 397.22,59.91 401.86,60.89 406.50,61.85 411.15,62.80 415.79,63.73 420.43,64.64 425.07,65.54 429.71,66.42 434.36,67.29 439.00,68.14 443.64,68.98 448.28,69.81 452.92,70.63 457.56,71.43 462.21,72.22 466.85,73.00 471.49,73.77 476.13,74.53 480.77,75.28 485.42,76.02 490.06,76.74 494.70,77.46 499.34,78.17 503.98,78.87 508.63,79.57 513.27,80.25 517.91,80.93 522.55,81.59 527.19,82.25 531.83,82.90 536.48,83.55 541.12,84.19 545.76,84.82 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<polyline points='81.57,1105.84 86.22,820.51 90.86,695.61 95.50,621.39 100.14,570.77 104.78,533.38 109.42,504.29 114.07,480.81 118.71,461.33 123.35,444.82 127.99,430.59 132.63,418.15 137.28,407.16 141.92,397.34 146.56,388.50 151.20,380.49 155.84,373.18 160.49,366.47 165.13,360.28 169.77,354.55 174.41,349.22 179.05,344.25 183.69,339.59 188.34,335.22 192.98,331.10 197.62,327.22 202.26,323.54 206.90,320.05 211.55,316.74 216.19,313.58 220.83,310.57 225.47,307.70 230.11,304.95 234.75,302.31 239.40,299.79 244.04,297.36 248.68,295.02 253.32,292.78 257.96,290.61 262.61,288.52 267.25,286.50 271.89,284.55 276.53,282.66 281.17,280.83 285.82,279.06 290.46,277.34 295.10,275.67 299.74,274.05 304.38,272.47 309.02,270.94 313.67,269.45 318.31,268.00 322.95,266.58 327.59,265.20 332.23,263.86 336.88,262.54 341.52,261.26 346.16,260.01 350.80,258.78 355.44,257.58 360.09,256.41 364.73,255.26 369.37,254.14 374.01,253.04 378.65,251.96 383.29,250.91 387.94,249.87 392.58,248.85 397.22,247.86 401.86,246.88 406.50,245.91 411.15,244.97 415.79,244.04 420.43,243.13 425.07,242.23 429.71,241.35 434.36,240.48 439.00,239.63 443.64,238.79 448.28,237.96 452.92,237.14 457.56,236.34 462.21,235.55 466.85,234.77 471.49,234.00 476.13,233.24 480.77,232.49 485.42,231.75 490.06,231.02 494.70,230.30 499.34,229.59 503.98,228.89 508.63,228.20 513.27,227.52 517.91,226.84 522.55,226.18 527.19,225.52 531.83,224.86 536.48,224.22 541.12,223.58 545.76,222.95 ' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<line x1='69.84' y1='206.46' x2='91.44' y2='206.46' style='stroke-width: 0.75; stroke: #9E9E9E; stroke-dasharray: 4.00,4.00;' />
<text x='94.14' y='210.76' style='font-size: 12.00px;fill: #9E9E9E; font-family: "Arial";' textLength='84.33px' lengthAdjust='spacingAndGlyphs'>Cook's distance</text>
</g>
<g clip-path='url(#cpMC4wMHw1NzYuMDB8MC4wMHwyODguMDA=)'>
<line x1='545.76' y1='214.56' x2='545.76' y2='84.82' style='stroke-width: 0.75;' />
<line x1='545.76' y1='202.72' x2='545.76' y2='202.72' style='stroke-width: 0.75;' />
<line x1='545.76' y1='105.05' x2='545.76' y2='105.05' style='stroke-width: 0.75;' />
<line x1='545.76' y1='84.82' x2='545.76' y2='84.82' style='stroke-width: 0.75;' />
<text x='549.36' y='205.94' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='12.50px' lengthAdjust='spacingAndGlyphs'>0.5</text>
<text x='549.36' y='108.27' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='12.50px' lengthAdjust='spacingAndGlyphs'>0.5</text>
<text x='549.36' y='88.04' style='font-size: 9.00px;fill: #9E9E9E; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>1</text>
<text x='302.40' y='52.56' text-anchor='middle' style='font-size: 12.00px; font-family: "Arial";' textLength='121.39px' lengthAdjust='spacingAndGlyphs'>Residuals vs Leverage</text>
<text x='477.70' y='76.88' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>3</text>
<text x='342.02' y='106.62' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>1</text>
<text x='375.63' y='199.45' text-anchor='end' style='font-size: 9.00px; font-family: "Arial";' textLength='5.00px' lengthAdjust='spacingAndGlyphs'>8</text>
</g>
</g>
</svg>
*)
(**
That's it - while simple, we hope this example illustrate
how you would go about to use any existing R statistical package.
While the details would differ, the general approach would
remain the same. Happy modelling!

*)