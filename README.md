Download Link: https://assignmentchef.com/product/solved-homework-3-smoothing-sta442-methods-of-applied-statistics
<br>
<h1>1          CO2</h1>

Figure 1 shows atmoshperic Carbon Dioxide concentrations from an observatory in Haiwaii, made available by the Scripps CO<sub>ϵ </sub>Program at <a href="http://scrippsco2.ucsd.edu/">scrippsco2.ucsd.edu</a>. The figure was produced with code in the appendix.

Figure 1: CO2 at Mauna Loa Observatory, Hawaii

Write a short consulting report (roughly a page of writing) discussing if the CO2 data appears to be impacted by the following events:

<ul>

 <li>the OPEC oil embargo which began in October 1973;</li>

 <li>the global economic recessions around 1980-1982;</li>

 <li>the fall of the Berlin wall almost exactly 30 years ago, preceding a dramatic fall in industrial production in the Soviet Union and Eastern Europe;</li>

 <li>China joining the WTO on 11 December 2001, which was followed by rapid growth in industrial production;</li>

 <li>the bankruptcy of Lehman Brothers on 15 September 2008, regarded as the symbolic start of the most recent global financial crisis; and</li>

 <li>the signing of the Paris Agreement on 12 December 2015, intended to limit CO2 emissions.</li>

</ul>

This last event is particularly important, as it suggests the growth rate of CO2 in the atmosphere should be lower now that it has been in the recent past.

You should

<ul>

 <li>explain fully the model you are using and why you have chosen to use it</li>

 <li>make your graphs look nice</li>

 <li>at a minimum, plot the estimated smoothed trend of CO2 and discuss whether it appears shallower or steeper after the events listed above.</li>

 <li>you could consider estimating the derivative of the trend. The help files for gam show how to do this, and some code doing it in inla are in the appendix.</li>

 <li>visual investigation is sufficient, you aren’t expected for formally test for effects.</li>

</ul>

<h1>2          Heat</h1>

Figure 2 a shows daily maximum temperature data recorded on Sable Island, off the coast of Nova Scotia. Figure 2 b shows the period from 2016 to the present, with summer months (May to October inclusive) in black and winter in red. Figure 2 c shows an estimated time trend produced using code in the appendix, and Figure 2 d shows posterior samples of this trend. Notice that the winter temperatures are more variable than summer temperatures, and you can assume you have been advised by a reliable environmental scientist that it is advisable to consider only summer temperatures when modelling historical temperature time series (since winter temperatures are governed by a different and much more complex physical process).

The IPCC states

Human activities are estimated to have caused approximately 1.0°C of global warming above preindustrial levels, with a likely range of 0.8°C to 1.2°C. Global warming is likely to reach 1.5°C between 2030 and 2052 if it continues to increase at the current rate. (high confidence)

see <a href="https://www.ipcc.ch/sr15/resources/headline-statements/">www.ipcc.ch/sr15/resources/headline-statements</a>

Your task is to prepare a short report (at most 2 pages of writing) discussing whether the data from Sable Island is broadly supportive of this statement from the IPCC. If you wish, you could write a report as a response to the following letter.

To: You

From: Maxim Burningier

Dear highly talented Statistician,

As you are no doubt aware, my political party believes that “Climate change alarmism is based on flawed models that have consistently failed at correctly predicting the future.” and “CO2 is beneficial for agriculture and there has recently been a measurable “greening” of the world in part thanks to higher levels.” (see <a href="https://www.peoplespartyofcanada.ca/global_warming_and_environment_rejecting_alarmism_and_focusing_on_concrete_improvements">www.peoplespartyofcanada.ca/platform</a> ). I have made a simple scatterplot of temperature measured at Sable Island over time and see no relationship whatsoever. I would like to employ you to write a two-page consulting report using the Sable Island data to refute the IPCC’s irresponsible statements about global temperature rises. My enemies will no doubt give your report to deluded scientists who will comb over your methods and results looking for flaws, so you must clearly explain the model you are using and justify any model assumptions and prior distributions. Make your report self-contained so as to be understandable by someone who has not read the instructions I’m giving you. In return for this report I will compensate you with 100 barrels of unprocessed bitumen, a delightful substance which you may enjoy drinking with dinner or mixing into your bath water to assist with relaxation.

Many thanks in anticipation

Maxim-the-denier

(c) fit                                                                                              (d) samples

Figure 2: Sable island temperature data

<h1>Appendix</h1>

<h2>CO2</h2>

cUrl = <strong>paste0</strong>(“http://scrippsco2.ucsd.edu/assets/data/atmospheric/”,

“stations/flask_co2/daily/daily_flask_co2_mlo.csv”) cFile = <strong>basename</strong>(cUrl) <strong>if </strong>(!<strong>file.exists</strong>(cFile)) <strong>download.file</strong>(cUrl, cFile) co2s = <strong>read.table</strong>(cFile, header = FALSE, sep = “,”, skip = 69, stringsAsFactors = FALSE, col.names = <strong>c</strong>(“day”,

“time”, “junk1”, “junk2”, “Nflasks”, “quality”,

“co2”)) co2s$date = <strong>strptime</strong>(<strong>paste</strong>(co2s$day, co2s$time), format = “%Y-%m-%d %H:%M”,

tz = “UTC”)

<em># remove low-quality measurements </em>co2s[co2s$quality &gt;= 1, “co2”] = NA

<strong>plot</strong>(co2s$date, co2s$co2, log = “y”, cex = 0.3, col = “#00000040”,

xlab = “time”, ylab = “ppm”)

<strong>plot</strong>(co2s[co2s$date &gt; <strong>ISOdate</strong>(2015, 3, 1, tz = “UTC”),

<strong>c</strong>(“date”, “co2”)], log = “y”, type = “o”, xlab = “time”, ylab = “ppm”, cex = 0.5)

The code below might prove useful.

timeOrigin = <strong>ISOdate</strong>(1980, 1, 1, 0, 0, 0, tz = “UTC”) co2s$days = <strong>as.numeric</strong>(<strong>difftime</strong>(co2s$date, timeOrigin,

units = “days”)) co2s$cos12 = <strong>cos</strong>(2 * pi * co2s$days/365.25) co2s$sin12 = <strong>sin</strong>(2 * pi * co2s$days/365.25) co2s$cos6 = <strong>cos</strong>(2 * 2 * pi * co2s$days/365.25) co2s$sin6 = <strong>sin</strong>(2 * 2 * pi * co2s$days/365.25) cLm = <strong>lm</strong>(co2 ~ days + cos12 + sin12 + cos6 + sin6,

data = co2s)

<strong>summary</strong>(cLm)$coef[, 1:2]

Estimate Std. Error (Intercept) 337.499286660 1.027025e-01 days     0.004651719 1.277835e-05 cos12      -0.898874589 9.274641e-02 sin12 2.884495702 9.153367e-02 cos6 0.657621761 9.245452e-02 sin6 -0.613041747 9.181487e-02 newX = <strong>data.frame</strong>(date = <strong>seq</strong>(<strong>ISOdate</strong>(1990, 1, 1, 0,

0, 0, tz = “UTC”), by = “1 days”, length.out = 365 *

30)) newX$days = <strong>as.numeric</strong>(<strong>difftime</strong>(newX$date, timeOrigin,

units = “days”)) newX$cos12 = <strong>cos</strong>(2 * pi * newX$days/365.25) newX$sin12 = <strong>sin</strong>(2 * pi * newX$days/365.25) newX$cos6 = <strong>cos</strong>(2 * 2 * pi * newX$days/365.25) newX$sin6 = <strong>sin</strong>(2 * 2 * pi * newX$days/365.25) coPred = <strong>predict</strong>(cLm, newX, se.fit = TRUE) coPred = <strong>data.frame</strong>(est = coPred$fit, lower = coPred$fit 2 * coPred$se.fit, upper = coPred$fit + 2 * coPred$se.fit) <strong>plot</strong>(newX$date, coPred$est, type = “l”) <strong>matlines</strong>(<strong>as.numeric</strong>(newX$date), coPred[, <strong>c</strong>(“lower”,

“upper”, “est”)], lty = 1, col = <strong>c</strong>(“yellow”, “yellow”,

“black”)) newX = newX[1:365, ] newX$days = 0

<strong>plot</strong>(newX$date, <strong>predict</strong>(cLm, newX))

(a) forecast                                                                       (b) cycle

Figure 3: Results

<h3>library(“INLA”)</h3>

<em># time random effect </em>timeBreaks = <strong>seq</strong>(<strong>min</strong>(co2s$date), <strong>ISOdate</strong>(2025, 1, 1,

tz = “UTC”), by = “14 days”)

timePoints = timeBreaks[-1]

co2s$timeRw2 = <strong>as.numeric</strong>(<strong>cut</strong>(co2s$date, timeBreaks))

<em># derivatives of time random effect</em>

D = <strong>Diagonal</strong>(<strong>length</strong>(timePoints)) – <strong>bandSparse</strong>(<strong>length</strong>(timePoints), k = -1)

derivLincomb = <strong>inla.make.lincombs</strong>(timeRw2 = D[-1, ]) <strong>names</strong>(derivLincomb) = <strong>gsub</strong>(“^lc”, “time”, <strong>names</strong>(derivLincomb))

<em># seasonal effect</em>

StimeSeason = <strong>seq</strong>(<strong>ISOdate</strong>(2009, 9, 1, tz = “UTC”), <strong>ISOdate</strong>(2011, 3, 1, tz = “UTC”), len = 1001)

StimeYear = <strong>as.numeric</strong>(<strong>difftime</strong>(StimeSeason, timeOrigin,

“days”))/365.35 seasonLincomb = <strong>inla.make.lincombs</strong>(sin12 = <strong>sin</strong>(2 * pi * StimeYear), cos12 = <strong>cos</strong>(2 * pi * StimeYear), sin6 = <strong>sin</strong>(2 * 2 * pi * StimeYear), cos6 = <strong>cos</strong>(2 *

2 * pi * StimeYear)) <strong>names</strong>(seasonLincomb) = <strong>gsub</strong>(“^lc”, “season”, <strong>names</strong>(seasonLincomb))

<em># predictions</em>

StimePred = <strong>as.numeric</strong>(<strong>difftime</strong>(timePoints, timeOrigin, units = “days”))/365.35

<h3>predLincomb = inla.make.lincombs(timeRw2 = Diagonal(length(timePoints)),</h3>

`(Intercept)` = <strong>rep</strong>(1, <strong>length</strong>(timePoints)), sin12 = <strong>sin</strong>(2 *

pi * StimePred), cos12 = <strong>cos</strong>(2 * pi * StimePred),

sin6 = <strong>sin</strong>(2 * 2 * pi * StimePred), cos6 = <strong>cos</strong>(2 *

2 * pi * StimePred)) <strong>names</strong>(predLincomb) = <strong>gsub</strong>(“^lc”, “pred”, <strong>names</strong>(predLincomb))

StimeIndex = <strong>seq</strong>(1, <strong>length</strong>(timePoints))

timeOriginIndex = <strong>which.min</strong>(<strong>abs</strong>(<strong>difftime</strong>(timePoints, timeOrigin)))

<em># disable some error checking in INLA</em>

<h3>library(“INLA”) mm = get(“inla.models”, INLA:::inla.get.inlaEnv())</h3>

<strong>if</strong>(<strong>class</strong>(mm) == ‘function’) mm = <strong>mm</strong>() mm$latent$rw2$min.diff = NULL

<h3>assign(“inla.models”, mm, INLA:::inla.get.inlaEnv())</h3>

co2res = <strong>inla</strong>(co2 ~ sin12 + cos12 + sin6 + cos6


<strong>f</strong>(timeRw2, model = ‘rw2′,

values = StimeIndex,

prior=’pc.prec’, param = <strong>c</strong>(<strong>log</strong>(1.01)/26, 0.5)),

data = co2s, family=’gamma’, lincomb = <strong>c</strong>(derivLincomb, seasonLincomb, predLincomb), control.family = <strong>list</strong>(hyper=<strong>list</strong>(prec=<strong>list</strong>(prior=’pc.prec’, param=<strong>c</strong>(2, 0.5)))),

<em># add this line if your computer has trouble</em>

<em># control.inla = list(strategy=’gaussian’, int.strategy=’eb’), </em>verbose=TRUE)

<strong>matplot</strong>(timePoints, <strong>exp</strong>(co2res$summary.random$timeRw2[, <strong>c</strong>(“0.5quant”, “0.025quant”, “0.975quant”)]), type = “l”, col = “black”, lty = <strong>c</strong>(1, 2, 2), log = “y”, xaxt = “n”,

xlab = “time”, ylab = “ppm”)

xax = <strong>pretty</strong>(timePoints) <strong>axis</strong>(1, xax, <strong>format</strong>(xax, “%Y”))

derivPred = co2res$summary.lincomb.derived[<strong>grep</strong>(“time”,

<strong>rownames</strong>(co2res$summary.lincomb.derived)), <strong>c</strong>(“0.5quant”,

“0.025quant”, “0.975quant”)] scaleTo10Years = (10 * 365.25/<strong>as.numeric</strong>(<strong>diff</strong>(timePoints,

units = “days”)))

<strong>matplot</strong>(timePoints[-1], scaleTo10Years * derivPred,

type = “l”, col = “black”, lty = <strong>c</strong>(1, 2, 2), ylim = <strong>c</strong>(0,

0.1), xlim = <strong>range</strong>(<strong>as.numeric</strong>(co2s$date)), xaxs = “i”, xaxt = “n”, xlab = “time”, ylab = “log ppm, change per 10yr”)

<strong>axis</strong>(1, xax, <strong>format</strong>(xax, “%Y”))

<strong>abline</strong>(v = <strong>ISOdate</strong>(2008, 1, 1, tz = “UTC”), col = “blue”) <strong>matplot</strong>(StimeSeason, <strong>exp</strong>(co2res$summary.lincomb.derived[<strong>grep</strong>(“season”,

<strong>rownames</strong>(co2res$summary.lincomb.derived)), <strong>c</strong>(“0.5quant”, “0.025quant”, “0.975quant”)]), type = “l”, col = “black”, lty = <strong>c</strong>(1, 2, 2), log = “y”, xaxs = “i”, xaxt = “n”,

xlab = “time”, ylab = “relative ppm”)

xaxSeason = <strong>seq</strong>(<strong>ISOdate</strong>(2009, 9, 1, tz = “UTC”), by = “2 months”,

len = 20)

<strong>axis</strong>(1, xaxSeason, <strong>format</strong>(xaxSeason, “%b”)) timePred = co2res$summary.lincomb.derived[<strong>grep</strong>(“pred”,

<strong>rownames</strong>(co2res$summary.lincomb.derived)), <strong>c</strong>(“0.5quant”,

“0.025quant”, “0.975quant”)] <strong>matplot</strong>(timePoints, <strong>exp</strong>(timePred), type = “l”, col = “black”,

lty = <strong>c</strong>(1, 2, 2), log = “y”, xlim = <strong>ISOdate</strong>(<strong>c</strong>(2010,

2025), 1, 1, tz = “UTC”), ylim = <strong>c</strong>(390, 435), xaxs = “i”, xaxt = “n”, xlab = “time”, ylab = “ppm”)

xaxPred = <strong>seq</strong>(<strong>ISOdate</strong>(2010, 1, 1, tz = “UTC”), by = “5 years”,

len = 20) <strong>axis</strong>(1, xaxPred, <strong>format</strong>(xaxPred, “%Y”))

Figure 4: INLA results

<h2>Heat</h2>

heatUrl = “http://pbrown.ca/teaching/appliedstats/data/sableIsland.rds” heatFile = <strong>tempfile</strong>(<strong>basename</strong>(heatUrl)) <strong>download.file</strong>(heatUrl, heatFile) x = <strong>readRDS</strong>(heatFile)

x$month = <strong>as.numeric</strong>(<strong>format</strong>(x$Date, “%m”)) xSub = x[x$month %in% 5:10 &amp; !<strong>is.na</strong>(x$Max.Temp…C.),

] weekValues = <strong>seq</strong>(<strong>min</strong>(xSub$Date), <strong>ISOdate</strong>(2030, 1, 1,

0, 0, 0, tz = “UTC”), by = “7 days”) xSub$week = <strong>cut</strong>(xSub$Date, weekValues) xSub$weekIid = xSub$week xSub$day = <strong>as.numeric</strong>(<strong>difftime</strong>(xSub$Date, <strong>min</strong>(weekValues),

units = “days”)) xSub$cos12 = <strong>cos</strong>(xSub$day * 2 * pi/365.25) xSub$sin12 = <strong>sin</strong>(xSub$day * 2 * pi/365.25) xSub$cos6 = <strong>cos</strong>(xSub$day * 2 * 2 * pi/365.25) xSub$sin6 = <strong>sin</strong>(xSub$day * 2 * 2 * pi/365.25) xSub$yearFac = <strong>factor</strong>(<strong>format</strong>(xSub$Date, “%Y”)) lmStart = <strong>lm</strong>(Max.Temp…C. ~ sin12 + cos12 + sin6


cos6, data = xSub)

startingValues = <strong>c</strong>(lmStart$fitted.values, <strong>rep</strong>(lmStart$coef[1],

<strong>nlevels</strong>(xSub$week)), <strong>rep</strong>(0, <strong>nlevels</strong>(xSub$weekIid) + <strong>nlevels</strong>(xSub$yearFac)), lmStart$coef[-1]) INLA::inla.doc(‘^t$’) <strong>library</strong>(“INLA”)

mm = <strong>get</strong>(“inla.models”, INLA:::<strong>inla.get.inlaEnv</strong>()) <strong>if</strong>(<strong>class</strong>(mm) == ‘function’) mm = <strong>mm</strong>() mm$latent$rw2$min.diff = NULL

<h3>assign(“inla.models”, mm, INLA:::inla.get.inlaEnv())</h3>

sableRes = INLA::<strong>inla</strong>(

Max.Temp…C. ~ 0 + sin12 + cos12 + sin6 + cos6 + <strong>f</strong>(week, model=’rw2′, constr=FALSE, prior=’pc.prec’, param = <strong>c</strong>(0.1/(52*100), 0.05))


<strong>f</strong>(weekIid, model=’iid’, prior=’pc.prec’, param = <strong>c</strong>(1, 0.5))


<strong>f</strong>(yearFac, model=’iid’, prior=’pc.prec’,

param = <strong>c</strong>(1, 0.5)),

family=’T’, control.family = <strong>list</strong>(

hyper = <strong>list</strong>(

prec = <strong>list</strong>(prior=’pc.prec’, param=<strong>c</strong>(1, 0.5)), dof = <strong>list</strong>(prior=’pc.dof’, param=<strong>c</strong>(10, 0.5)))),

control.mode = <strong>list</strong>(theta = <strong>c</strong>(-1,2,20,0,1), x = startingValues, restart=TRUE),

control.compute=<strong>list</strong>(config = TRUE),

<em># control.inla = list(strategy=’gaussian’, int.strategy=’eb’), </em>data = xSub, verbose=TRUE) sableRes$summary.hyper[, <strong>c</strong>(4, 3, 5)]

0.5quant  0.025quant

precision for the student-t observations 3.211698e-01 3.141445e-01

<table width="460">

 <tbody>

  <tr>

   <td width="286">degrees of freedom for student-t</td>

   <td width="174">1.379119e+01 1.142332e+01</td>

  </tr>

  <tr>

   <td width="286">Precision for week</td>

   <td width="174">3.439837e+09 2.608100e+09</td>

  </tr>

  <tr>

   <td width="286">Precision for weekIid</td>

   <td width="174">8.335287e-01 7.665303e-01</td>

  </tr>

  <tr>

   <td width="286">Precision for yearFac</td>

   <td width="174">2.038099e+00 1.348922e+00</td>

  </tr>

 </tbody>

</table>

0.975quant

precision for the student-t observations 3.312381e-01

<table width="370">

 <tbody>

  <tr>

   <td width="286">degrees of freedom for student-t</td>

   <td width="84">1.673788e+01</td>

  </tr>

  <tr>

   <td width="286">Precision for week</td>

   <td width="84">4.543424e+09</td>

  </tr>

  <tr>

   <td width="286">Precision for weekIid</td>

   <td width="84">8.875598e-01</td>

  </tr>

  <tr>

   <td width="286">Precision for yearFac</td>

   <td width="84">2.753691e+00</td>

  </tr>

 </tbody>

</table>

sableRes$summary.fixed[, <strong>c</strong>(4, 3, 5)]

0.5quant 0.025quant 0.975quant

sin12 -4.6739279 -5.189943 -4.15843811 cos12 4.7607402 4.470423 5.05090575 sin6 -2.0499222 -2.273244 -1.82670780 cos6 -0.1529845 -0.324455 0.01837625

Pmisc::<strong>priorPost</strong>(sableRes)$summary[, <strong>c</strong>(1, 3, 5)]

mean 0.025quant  0.975quant

sd for t    1.763458e+00 1.737520e+00 1.784166e+00 sd for week  1.709116e-05 1.483571e-05 1.958113e-05 sd for weekIid 1.097193e+00 1.061454e+00 1.142182e+00 sd for yearFac 7.091899e-01 6.026184e-01 8.610067e-01 mySample = <strong>inla.posterior.sample</strong>(n = 24, result = sableRes,

num.threads = 8, selection = <strong>list</strong>(week = <strong>seq</strong>(1,

<strong>nrow</strong>(sableRes$summary.random$week))))

<strong>length</strong>(mySample) <strong>names</strong>(mySample[[1]])

weekSample = <strong>do.call</strong>(cbind, <strong>lapply</strong>(mySample, <strong>function</strong>(xx) xx$latent)) <strong>dim</strong>(weekSample) <strong>head</strong>(weekSample)

<strong>plot</strong>(x$Date, x$Max.Temp…C., col = mapmisc::<strong>col2html</strong>(“black”,

0.3)) forAxis = <strong>ISOdate</strong>(2016:2020, 1, 1, tz = “UTC”) <strong>plot</strong>(x$Date, x$Max.Temp…C., xlim = <strong>range</strong>(forAxis),

xlab = “time”, ylab = “degrees C”, col = “red”, xaxt = “n”)

<strong>points</strong>(xSub$Date, xSub$Max.Temp…C.) <strong>axis</strong>(1, forAxis, <strong>format</strong>(forAxis, “%Y”)) <strong>matplot</strong>(weekValues[-1], sableRes$summary.random$week[, <strong>paste0</strong>(<strong>c</strong>(0.5, 0.025, 0.975), “quant”)], type = “l”, lty = <strong>c</strong>(1, 2, 2), xlab = “time”, ylab = “degrees C”, xaxt = “n”, col = “black”, xaxs = “i”)

forXaxis2 = <strong>ISOdate</strong>(<strong>seq</strong>(1880, 2040, by = 20), 1, 1,

tz = “UTC”)

<strong>axis</strong>(1, forXaxis2, <strong>format</strong>(forXaxis2, “%Y”)) myCol = mapmisc::<strong>colourScale</strong>(NA, breaks = 1:8, style = “unique”,

col = “Set2”, opacity = 0.3)$col

<strong>matplot</strong>(weekValues[-1], weekSample, type = “l”, lty = 1,

col = myCol, xlab = “time”, ylab = “degrees C”, xaxt = “n”, xaxs = “i”) <strong>axis</strong>(1, forXaxis2, <strong>format</strong>(forXaxis2, “%Y”))