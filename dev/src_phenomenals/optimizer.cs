using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.PortableExecutable;
using System.Runtime.CompilerServices;
using phenologyRunner.data;
using phenologyRunner.dataStructure;
using phenologyRunner.dataStructure.Plant;
using phenologyRunner.models.Plant;
using phenologyRunner.Utilities;
using UNIMI.optimizer;

namespace runner
{
    //this class perform the multi-start simplex optimization of SWELL parameters
    internal class optimizer : IOBJfunc
    {
        #region optimizer methods
        int _neval = 0;
        int _ncompute = 0;

        public Dictionary<int, string> _Phenology = new Dictionary<int, string>();
        
        // the number of times that this function is called        
        public int neval
        {
            get
            {
                return _neval;
            }

            set
            {
                _neval = value;
            }
        }

        
        // the number of times where the function is evaluated 
        // (when an evaluation is requested outside the parameters domain this counter is not incremented        
        public int ncompute
        {
            get
            {
                return _ncompute;
            }

            set
            {
                _ncompute = value;
            }
        }
        #endregion

        #region instances of data types and functions
        //saetta data type
        outputs output = new outputs();
        outputs outputT1 = new outputs();
        //instance of the saetta functions
       
        Dormancy _Dormancy = new Dormancy();
        Forcing _Forcing = new Forcing();
        Phenomenals _Phenomenals = new Phenomenals();
        Transpiration _Transpiration = new Transpiration();
       #endregion

        #region instance of the weather and parameter reader class
        weatherReader weatherReader = new weatherReader();
        paramReader paramReader = new paramReader();
        #endregion

        #region local variables to perform the optimization
        public Dictionary<string, simulationUnit> site_simulationUnit = new Dictionary<string, simulationUnit>();
        public List<string> availableSites = new List<string>();
        public Dictionary<string, parameter> nameParam = new Dictionary<string, parameter>();
        public Dictionary<string, float> param_outCalibration = new Dictionary<string, float>();
        public string caseStudy;
        public Dictionary<DateTime, outputs> date_outputs = new Dictionary<DateTime, outputs>();
        public string weatherDir;
        public string plantParamFile;
        public List<string> allWeatherDataFiles;
        public string variety;    
        public bool isCalibration;  
        //for validation
        public string country;
        public int parset;
        public int startYear;
        public int endYear;
        public int iterations;
        public string weatherTimeStep;
        int barWidth = 30; // Width of the progress bar
        public float currentPixelNumber;
        public ConsoleColor consoleColor;

        #endregion

        //this method perform the multi-start simplex calibration
        public double ObjfuncVal(double[] Coefficient, double[,] limits)
        {
            #region Calibration parameter validation
            for (int j = 0; j < Coefficient.Length; j++)
            {
                if (Coefficient[j] == 0)
                {
                    break;
                }
                if (Coefficient[j] <= limits[j, 0] | Coefficient[j] > limits[j, 1])
                {
                    return 1E+300;
                }

            }
            _neval++;
            _ncompute++;
            #endregion

            #region Assign parameters
            var parameters = new parameters();
            var parGenotype = new phenomenalsParameters();
            var propsGenotype = parGenotype.GetType().GetProperties();

            var parPhenology = new phenologyParameters();
            var propsPhenology = parPhenology.BBCHParameters.GetType().GetProperties();

            int i = 0;

            foreach (var param in nameParam.Keys)
            {
                string[] paramClass = param.Split('_');
                string className = paramClass[0];
                string propertyName = paramClass[1];

                bool isCalibrated = nameParam[param].calibration != "";

                if (className == "phenomenalsParameters")
                {
                    var prop = propsGenotype.FirstOrDefault(p => p.Name == propertyName);
                    if (prop != null)
                        prop.SetValue(parGenotype, isCalibrated ? (float)Coefficient[i++] : param_outCalibration[param]);
                }
                else if (className == "BBCHParameters")
                {
                    int bbchCode = int.Parse(propertyName.Substring(4, 2));
                    if (!parameters.phenologyParameters.BBCHParameters.ContainsKey(bbchCode))
                        parameters.phenologyParameters.BBCHParameters[bbchCode] = new BBCH_Parameter();

                    var bbchParam = parameters.phenologyParameters.BBCHParameters[bbchCode];
                    if (bbchParam.cycleCompletion == 0)
                        bbchParam.cycleCompletion = isCalibrated ? (int)Coefficient[i++] : (int)param_outCalibration[param];
                }
            }

            parameters.phenologyParameters.phenomenalsParameters = parGenotype;
            parameters.phenologyParameters.BBCHParameters = parameters.phenologyParameters.BBCHParameters
                .OrderBy(kvp => kvp.Key)
                .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

            parameters.phenologyParameters = generateDetailedPhenologyParameters(parameters.phenologyParameters);
            #endregion

            #region Model execution and error calculation
            List<double> errors = new List<double>();
            Dictionary<int, Dictionary<int, DateTime>> year_simulatedBbch_doy = new();

            foreach (var dictKey in site_simulationUnit.Keys)
            {
                output = new outputs();
                outputT1 = new outputs();

                string site = dictKey.Split('_')[0];
                string weatherFile = Path.Combine(weatherDir, weatherTimeStep, site + ".csv");

                if (!File.Exists(weatherFile) || !availableSites.Contains(site) || site_simulationUnit[dictKey].variety != variety)
                    continue;

                Dictionary<DateTime, inputPlant> weatherData = weatherTimeStep switch
                {
                    "hourly" => weatherReader.readHourly(weatherFile, startYear, endYear, site),
                    "daily" => weatherReader.readDaily(weatherFile, startYear, endYear),
                    _ => new Dictionary<DateTime, inputPlant>()
                };

                // Now you can access:
                int startYearAvailable = weatherReader.startAvailableYear;
                int endYearAvailable = weatherReader.endAvailableYear;

                foreach (var hour in weatherData.Keys)
                {
                    if (hour.Year < startYear || hour.Year > endYear)
                    {
                        output = new outputs();
                        outputT1 = new outputs();
                        continue;
                    }

                    weatherData[hour].date = hour;
                    weatherData[hour].location.latitude = site_simulationUnit[dictKey].latitude;

                    int resetDay = (weatherData[hour].location.latitude > 0) ? 325 : 121;
                    if (hour.DayOfYear == resetDay)
                    {
                        output = outputT1;
                        outputT1 = new outputs();
                    }

                    modelCall(weatherData[hour], parameters, site);

                    int year = hour.Year;
                    int bbchPhase = outputT1.cropOutputs.bbchPhenophase;

                    if (!year_simulatedBbch_doy.ContainsKey(year))
                        year_simulatedBbch_doy[year] = new Dictionary<int, DateTime>();

                    if (!year_simulatedBbch_doy[year].ContainsKey(bbchPhase))
                        year_simulatedBbch_doy[year][bbchPhase] = hour;
                }

               errors = new List<double>();

                foreach (var year in year_simulatedBbch_doy.Keys)
                {
                    if (!year_simulatedBbch_doy[year].ContainsKey(89) && year > startYearAvailable)
                        errors.Add(999999999);
                }

                foreach (var year in site_simulationUnit[dictKey].referenceData.year_BBCH_date.Keys)
                {
                    if (year >= startYear && year <= endYear)
                    {
                        var refData = site_simulationUnit[dictKey].referenceData;
                        site = dictKey;

                        // Define Southern Hemisphere sites
                        bool isSouthernHemisphere = site_simulationUnit[dictKey].latitude < 0;

                        foreach (var bbch in refData.year_BBCH_date[year].Keys)
                        {
                            DateTime refDate = refData.year_BBCH_date[year][bbch];
                            int seasonalYear = GetSeasonalYear(refDate, isSouthernHemisphere);

                            // Gather all potential simulation matches
                            List<DateTime> possibleSimDates = new List<DateTime>();

                            // Look for simulated BBCH in seasonally matched years
                            for (int offset = -1; offset <= 1; offset++)
                            {
                                int simYear = seasonalYear + offset;

                                if (year_simulatedBbch_doy.ContainsKey(simYear) &&
                                    year_simulatedBbch_doy[simYear].ContainsKey(bbch))
                                {
                                    possibleSimDates.Add(year_simulatedBbch_doy[simYear][bbch]);
                                }
                            }

                            // Compare only if simulated dates are found
                            if (possibleSimDates.Count > 0)
                            {
                                // Get the simulated date closest to the reference
                                DateTime simDate = possibleSimDates
                                    .OrderBy(d => Math.Abs((d - refDate).TotalDays))
                                    .First();

                                double diffDays = (refDate - simDate).TotalDays;
                                errors.Add(Math.Pow(diffDays, 2));
                            }
                        }
                    }
                }

            }

            #endregion

            #region Objective function and console output
            // Calculate the progress percentage
            double progress = (float)_neval / ((float)iterations * Coefficient.Length);
            int progressBlocks = (int)(progress * barWidth) + 1;
            string progressBar = new string('█', progressBlocks).PadRight(barWidth, ' ');

            double objFun = errors.Count > 0 ? Math.Sqrt(errors.Sum() / errors.Count) : 999999999;
            string rmseStr = objFun.ToString("0.0").PadLeft(5);
            string msg = $"\rRMSE (days): {rmseStr}".PadRight(80);
            Console.ForegroundColor = consoleColor;
            Console.Write(msg + "| {0}", progressBar);
            Console.Out.Flush();
            #endregion

            return objFun;
        }

        int GetSeasonalYear(DateTime date, bool isSouthernHemisphere)
        {
            return isSouthernHemisphere && date.Month < 7 ? date.Year - 1 : date.Year;
        }

        //this method is called in the validation run
        public void oneShot(Dictionary<string, float> paramValue, out Dictionary<DateTime, outputs> date_outputs)
        {
            //reinitialize the date_outputs object
            date_outputs = new Dictionary<DateTime, outputs>();
            Dictionary<DateTime, outputs> date_outputs_hourly = new Dictionary<DateTime, outputs>();

            #region assign parameters
            phenologyRunner.dataStructure.Plant.parameters parameters = new parameters();
            phenomenalsParameters parGenotype = new phenomenalsParameters();
            PropertyInfo[] propsGenotype = parGenotype.GetType().GetProperties();//get all properties
            phenologyParameters parPhenology = new phenologyParameters();
            PropertyInfo[] propsPhenology = parPhenology.BBCHParameters.GetType().GetProperties();//get all properties 
            parameters = new parameters();

            //assign calibrated parameters
            foreach (var param in paramValue.Keys)
            {
                //split class from param name
                string[] paramClass = param.Split('_');

                if (paramClass[0] == "phenomenalsParameters")
                {
                    foreach (PropertyInfo prp in propsGenotype)
                    {
                        if (paramClass[1] == prp.Name)
                        {
                            prp.SetValue(parGenotype, (float)(paramValue[param])); //set the values for this parameter
                        }

                    }
                }
                if (paramClass[0] == "BBCHParameters")
                {
                    foreach (PropertyInfo prp in propsPhenology)
                    {
                        if (paramClass[1] != "bbch89")
                        {
                            parameters.phenologyParameters.BBCHParameters.Add(int.Parse(paramClass[1].Substring(4, 2)), new BBCH_Parameter());
                            if (parameters.phenologyParameters.BBCHParameters[int.Parse(paramClass[1].Substring(4, 2))].
                                cycleCompletion == 0)
                            {
                                parameters.phenologyParameters.BBCHParameters[int.Parse(paramClass[1].Substring(4, 2))].cycleCompletion =
                                    (float)(paramValue[param]); //set the values for this parameter
                            }
                            break;
                        }
                    }
                }
            }

            //assign parameters out of calibration
            foreach (var param in param_outCalibration.Keys)
            {
                //split class from param name
                string[] paramClass = param.Split('_');
                if (paramClass[0] == "phenomenalsParameters")
                {
                    foreach (PropertyInfo prp in propsGenotype)
                    {
                        if (paramClass[1] == prp.Name)
                        {
                            prp.SetValue(parGenotype, param_outCalibration[param]); //set the values for this parameter
                        }

                    }
                }
                if (paramClass[0] == "BBCHParameters")
                {
                    foreach (PropertyInfo prp in propsPhenology)
                    {
                        if (paramClass[1] != "bbch89")
                        {
                            if (!parameters.phenologyParameters.BBCHParameters.ContainsKey(int.Parse(paramClass[1].Substring(4, 2))))
                            {
                                parameters.phenologyParameters.BBCHParameters.Add(int.Parse(paramClass[1].Substring(4, 2)), new BBCH_Parameter());
                            }
                                if (parameters.phenologyParameters.BBCHParameters[int.Parse(paramClass[1].Substring(4, 2))].
                                cycleCompletion == 0)
                            {
                                parameters.phenologyParameters.BBCHParameters[int.Parse(paramClass[1].
                                    Substring(4, 2))].cycleCompletion = (int)(param_outCalibration[param]); //set the values for this parameter
                            }
                        }
                    }
                }
            }

            parameters.phenologyParameters.phenomenalsParameters = parGenotype;
            parameters.phenologyParameters.BBCHParameters = parameters.phenologyParameters.BBCHParameters.OrderBy(kvp => kvp.Key).ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
            phenologyParameters _detailedCropParameters = generateDetailedPhenologyParameters(parameters.phenologyParameters);
            parameters.phenologyParameters = _detailedCropParameters;
            #endregion

            //reinitialize variables for each site
            output = new outputs();
            outputT1 = new outputs();

            foreach (var dictKey in site_simulationUnit.Keys)
            {

                var site = dictKey.Split('_')[0];

                //weather file
                string weatherFile = weatherDir + "//" + weatherTimeStep + "//" + site + ".csv";

                // Check if the weather file exists
                if (File.Exists(weatherFile) && availableSites.Contains(site) && site_simulationUnit[dictKey].variety == variety)
                {
                    //read weather data
                    Dictionary<DateTime, phenologyRunner.dataStructure.Plant.inputPlant> weatherData =
                        new Dictionary<DateTime, inputPlant>();

                    if (weatherTimeStep == "hourly")
                    {
                        weatherData = weatherReader.readHourly(weatherFile, startYear, endYear, site);
                    }
                    else if (weatherTimeStep == "daily")
                    {
                        weatherData = weatherReader.readDaily(weatherFile, startYear, endYear);
                    }

                    //reinitialize the date_outputs object
                    date_outputs = new Dictionary<DateTime, outputs>();
                    date_outputs_hourly = new Dictionary<DateTime, outputs>();

                    //loop over dates
                    foreach (var hour in weatherData.Keys)
                    {
                        //set the simulation period: 
                        if (hour.Year >= startYear & hour.Year <= endYear)
                        {
                            weatherData[hour].date = hour;
                            weatherData[hour].location.latitude = site_simulationUnit[dictKey].latitude;
                            int resetDay = 0;
                            if (site_simulationUnit[dictKey].latitude > 0)
                            {
                                resetDay = 325;

                            }
                            else
                            {
                                resetDay = 121;
                            }

                            if (hour.DayOfYear == resetDay)
                            { 
                                output = new outputs();
                                outputT1 = new outputs();
                            }

                            modelCall(weatherData[hour], parameters, site);
                            date_outputs_hourly.Add(hour, outputT1);
                            date_outputs_hourly[hour].weatherInputHourly = weatherData[hour];
                            if (hour.Hour == 23)
                            {
                                //add the object to the output dictionary
                                date_outputs.Add(hour, outputT1);
                            }
                        }
                        else
                        {
                            output = new outputs();
                            outputT1 = new outputs();
                        }
                    }

                    #region write outputs file
                    if (isCalibration)
                    {
                       
                            //write the outputs from the calibration run
                            writeOutputsCalibration(site_simulationUnit[dictKey].latitude + "_" + site_simulationUnit[dictKey].longitude,
                                date_outputs, isCalibration, site, variety, date_outputs_hourly);
                        
                    }
                    else
                    {
                        //write the outputs from the validation run
                        writeOutputsValidationDaily(site_simulationUnit[dictKey].latitude + "_" + site_simulationUnit[dictKey].longitude,
                            date_outputs, site, variety, date_outputs_hourly);

                        //writeOutputsValidationHourly(site_simulationUnit[dictKey].latitude + "_" + site_simulationUnit[dictKey].longitude,
                        //    date_outputs_hourly, site, variety);
                    }
                    #endregion
                }
            }
        }

        #region write output files from calibration and validation

        //define the file header
        string header = "site,latitude,longitude,variety,Date,Tmax,Tmin,RHmax,RHmin,Rad,Prec,ET0,VPD,Wind,LW," +
    // Phenology model outputs
    "ChillState,AntiChillState,ForcingState,DormancyCompletion,CycleCompletion,BBCHCode,BBCHPhase,BBCHRef," +
 "TempF,HeatF,ColdF,LightF,AridityF,VPDefF,WindF,DiseaseF," +
 "TempFsum,HeatFsum,ColdFsum,LightFsum,AridityFsum,VPDefFsum,WindFsum,DiseaseFsum";


        //write outputs from the calibration run
        public void writeOutputsCalibration(string id, Dictionary<DateTime, outputs> date_outputs, 
            bool isCalibration, string site, string variety,
            Dictionary<DateTime, outputs> date_outputs_hourly)
        {
            if (isCalibration)
            {
                #region write outputs
                //empty list to store outputs
                List<string> toWrite = new List<string>();

                header = "site,latitude,longitude,variety,Date,"+
    "ChillState,AntiChillState,ForcingState,DormancyCompletion,CycleCompletion,BBCHCode,BBCHPhase,BBCHRef";


                //add the header to the list
                toWrite.Add(header);

                // Step 1: Group hourly data by date
                var hourlyGroupedByDate = date_outputs_hourly
                    .GroupBy(kvp => kvp.Key.Date)
                    .ToDictionary(g => g.Key, g => g.Select(kvp => kvp.Value).ToList());

                //loop over days
                foreach (var date in date_outputs.Keys)
                {
                    //empty string to store outputs
                    string line = "";

                    //populate this line
                    line += site + ",";
                    line += site_simulationUnit[site_simulationUnit.Keys.First()].latitude + ",";
                    line += site_simulationUnit[site_simulationUnit.Keys.First()].longitude + ",";
                    line += variety + ",";
                    line += date.Date.ToShortDateString() + ",";
                    //phenology
                    line += date_outputs[date].cropOutputs.chillState + ",";
                    line += date_outputs[date].cropOutputs.antiChillRate + ",";
                    line += date_outputs[date].cropOutputs.forcingState + ",";
                    line += date_outputs[date].cropOutputs.dormancyCompletionPercentage + ",";
                    line += date_outputs[date].cropOutputs.cycleCompletionPercentage + ",";
                    line += date_outputs[date].cropOutputs.bbchPhenophaseCode + ",";
                    line += date_outputs[date].cropOutputs.bbchPhenophase + ",";
                    if (site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date.ContainsKey(date.Year))
                    {
                        var dateCorrect = date_outputs[date].weatherInputDaily.date;
                        if (site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date[date.Year].ContainsValue(dateCorrect))
                        {
                            var value = site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date[date.Year]
        .FirstOrDefault(kvp => kvp.Value == dateCorrect).Key; // Get the matching value

                            line += value + "";
                        }
                        else
                        {
                            line += "";
                        }
                    }
                    else
                    {
                        line += "";
                    }

                    //add the line to the list
                    toWrite.Add(line);
                }

                // Get the directory where the executable is running
                string exeDir = AppDomain.CurrentDomain.BaseDirectory;

                // Build full path to calibratedVarieties folder
                string outputDir = Path.Combine(exeDir, "outputsCalibration_BBCH");

                // Ensure the folder exists
                Directory.CreateDirectory(outputDir);

                // Build the file path
                string fileName = $"{variety}_{site}.csv";
                string fullPath = Path.Combine(outputDir, fileName);

                File.WriteAllLines(fullPath, toWrite);
                
                #endregion
            }
        }

        //write outputs from the validation run
        public void writeOutputsValidationDaily(string id, Dictionary<DateTime, outputs> date_outputs, string  site, string variety,
            Dictionary<DateTime, outputs> date_outputs_hourly)
        {

            #region write outputs

            //empty list to store outputs
            List<string> toWrite = new List<string>();

            //add the header to the list
            toWrite.Add(header);

            // Step 1: Group hourly data by date
            var hourlyGroupedByDate = date_outputs_hourly
                .GroupBy(kvp => kvp.Key.Date)
                .ToDictionary(g => g.Key, g => g.Select(kvp => kvp.Value).ToList());


            //define the file header
            //loop over days
            foreach (var date in date_outputs.Keys)
            {
                //empty string to store outputs
                string line = "";

                //populate this line
                line += site + ",";
                line += site_simulationUnit[site_simulationUnit.Keys.First()].latitude + ",";
                line += site_simulationUnit[site_simulationUnit.Keys.First()].longitude + ",";
                line += variety + ",";
                line += date.Date.ToShortDateString() + ",";
                line += date_outputs[date].weatherInputDaily.temperatureMax + ",";
                line += date_outputs[date].weatherInputDaily.temperatureMin + ",";
                line += date_outputs[date].weatherInputDaily.relativeHumidityMax + ",";
                line += date_outputs[date].weatherInputDaily.relativeHumidityMin + ",";
                line += date_outputs[date].weatherInputDaily.radiation + ",";
                line += date_outputs[date].weatherInputDaily.precipitation + ",";
                line += date_outputs[date].weatherInputDaily.et0 + ",";
                line += date_outputs[date].weatherInputDaily.vpd + ",";
                line += date_outputs[date].weatherInputDaily.windSpeed + ",";
                line += date_outputs[date].weatherInputDaily.leafWetness + ",";
                //phenology
                line += date_outputs[date].cropOutputs.chillState + ",";
                line += date_outputs[date].cropOutputs.antiChillRate + ",";
                line += date_outputs[date].cropOutputs.forcingState + ",";
                line += date_outputs[date].cropOutputs.dormancyCompletionPercentage + ",";
                line += date_outputs[date].cropOutputs.cycleCompletionPercentage + ",";
                line += date_outputs[date].cropOutputs.bbchPhenophaseCode + ",";
                line += date_outputs[date].cropOutputs.bbchPhenophase + ",";
                if (site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date.ContainsKey(date.Year))
                {
                    var dateCorrect = date_outputs[date].weatherInputDaily.date;
                    if (site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date[date.Year].ContainsValue(dateCorrect))
                    {
                        var value = site_simulationUnit[site + "_" + variety].referenceData.year_BBCH_date[date.Year]
                            .FirstOrDefault(kvp => kvp.Value == dateCorrect).Key; // Get the matching value
                        line += value+ ",";
                    }
                    else { line += ",";}
                }
                else { line += ",";}
               
                // Get hourly data for that date
                if (hourlyGroupedByDate.ContainsKey(date.Date))
                {
                    var hourlyData = hourlyGroupedByDate[date.Date];
                
                    // Aggregates (choose per variable)
                    line += hourlyData.Average(h => h.cropOutputs.temperatureFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.heatStressFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.coldStressFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.lightFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.aridityFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.VPDeficitFunction) + ",";
                    line += hourlyData.Average(h => h.cropOutputs.windFunction) + ",";
                    line += hourlyData.Max(h => h.cropOutputs.diseaseFunction) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.temperatureFunctionState/24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.heatStressFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.coldStressFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.lightFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.aridityFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.VPDeficitFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.windFunctionState / 24) + ",";
                    line += hourlyData.Sum(h => h.cropOutputs.diseaseFunctionState / 24);
                }
                // No hourly data for this day? Fill blanks
                else { line += string.Join(",", Enumerable.Repeat("NA", 24)); }

                //add the line to the list
                toWrite.Add(line);
            }

            // Get the directory where the executable is running
            string exeDir = AppDomain.CurrentDomain.BaseDirectory;

            // Build full path to calibratedVarieties folder
            string outputDir = Path.Combine(exeDir, "outputsValidationDaily");

            // Ensure the folder exists
            Directory.CreateDirectory(outputDir);

            // Build the file path
            string fileName = $"{variety}_{site}.csv";
            string fullPath = Path.Combine(outputDir, fileName);

            File.WriteAllLines(fullPath, toWrite);

            #endregion
        }

        public void writeOutputsValidationHourly(string id, Dictionary<DateTime, outputs> date_outputs_hourly, string site, string variety)
        {
            #region write outputs
    //        //empty list to store outputs
    //        List<string> toWrite = new List<string>();

    //        //define the file header
    //        string header = "site,variety,Date,hour,Temp,RH,rad,radDaily,ET0,rain,wind,lw," +
    //    // Phenology model outputs
    //    "chillState,chillRate,antiChillState," +
    //    "forcingRate,forcingState,cycleCompletion," +
    //      "bbchCode,bbchPhase,bbchRef," +
    //      "temperatureCue,coldR,coldS,heatR,heatS,wsR,wsS," +
    //      "lightIntPot,wsImpact,lightIntAct," +
    //      "cropCoefficient,cropTranspiration," +
    //      "stomataLightF,stomataVPDF,stomataTF,stomataWsF,stomataCond," +
    //      "photoR,photoS," +
    //      "pollinationPotR,pollinationPotS,pollinationPrecLim,pollinationWindLim,pollinationLimR,pollinationLimS";

    //        //add the header to the list
    //        toWrite.Add(header);

    //        //loop over days
    //        foreach (var date in date_outputs_hourly.Keys)
    //        {
    //            //empty string to store outputs
    //            string line = "";

    //            //populate this line
    //            line += site + ",";
    //            line += variety + ",";
    //            line += date.Date + ",";
    //            line += date.Date.Hour + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.temperature + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.relativeHumidity + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.radiation + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.radData.gsr + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.et0 + ",";
    //            line += date_outputs_hourly[date].weatherInputHourly.weather.precipitation + ",";
    //            //phenology
    //            line += date_outputs_hourly[date].cropOutputs.chillState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.chillRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.antiChillRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.forcingRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.forcingState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.cycleCompletionPercentage + ",";
    //            line += date_outputs_hourly[date].cropOutputs.bbchPhenophaseCode + ",";
    //            line += date_outputs_hourly[date].cropOutputs.bbchPhenophase + ",";
    //            if (site_simulationUnit[site].referenceData.year_BBCH_date.ContainsKey(date.Year))
    //            {
    //                var dateCorrect = date_outputs_hourly[date].weatherInputDaily.date;
    //                if (site_simulationUnit[site].referenceData.year_BBCH_date[date.Year].ContainsValue(dateCorrect))
    //                {
    //                    var value = site_simulationUnit[site].referenceData.year_BBCH_date[date.Year]
    //.FirstOrDefault(kvp => kvp.Value == dateCorrect).Key; // Get the matching value

    //                    line += value + ",";
    //                }
    //                else
    //                {
    //                    line += ",";
    //                }
    //            }
    //            else
    //            {
    //                line += ",";
    //            }
    //            //STRESSES
    //            line += date_outputs_hourly[date].cropOutputs.TemperatureCue + ",";
    //            line += date_outputs_hourly[date].cropOutputs.coldStressRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.coldStressState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.heatStressRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.heatStressState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.waterStressRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.waterStressState+",";
    //            line += date_outputs_hourly[date].cropOutputs.lightInterceptionPotentialState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.waterStressImpact + ",";
    //            line += date_outputs_hourly[date].cropOutputs.lightInterceptionLimitedState+",";
    //            line += date_outputs_hourly[date].cropOutputs.cropCoefficient + ",";
    //            line += date_outputs_hourly[date].cropOutputs.cropTranspiration+",";
    //            line += date_outputs_hourly[date].cropOutputs.stomataLightEffect + ",";
    //            line += date_outputs_hourly[date].cropOutputs.stomataVPDeffect + ",";
    //            line += date_outputs_hourly[date].cropOutputs.stomataTemperatureEffect + ",";
    //            line += date_outputs_hourly[date].cropOutputs.stomataWaterStressEffect + ",";
    //            line += date_outputs_hourly[date].cropOutputs.stomataConductance + ",";
    //            line += date_outputs_hourly[date].cropOutputs.photosyntheticRate + ",";
    //            line += date_outputs_hourly[date].cropOutputs.photosyntheticState + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationPotentialR + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationPotentialS + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationPrecipitationLimitation + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationWindLimitation + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationLimitedR + ",";
    //            line += date_outputs_hourly[date].cropOutputs.pollinationLimitedS; ;


    //            //add the line to the list
    //            toWrite.Add(line);
    //        }
    //        //save the file
    //        System.IO.File.WriteAllLines(@"outputsValidationHourly//" + variety + "_" + site + ".csv", toWrite);
            #endregion
        }
       
        #endregion


        List<double> Temperatures = new List<double>();
        List<double> Precipitation = new List<double>();
        List<double> ET0 = new List<double>();
        List<double> VPD = new List<double>();
        List<double> Rad = new List<double>();
        List<double> RHs = new List<double>();
        List<double> LW = new List<double>();
        List<double> WindSpeed = new List<double>();

        public void modelCall(inputPlant weatherData, phenologyRunner.dataStructure.Plant.parameters parameters, string site)
        {
            //call the functions
            output = outputT1;
            outputT1 = new outputs();
            outputT1.cropOutputs.ET0memory = output.cropOutputs.ET0memory;
            outputT1.cropOutputs.PrecipitationMemory = output.cropOutputs.PrecipitationMemory;

            //phenology
            Temperatures.Add(weatherData.weather.temperature);
            Precipitation.Add(weatherData.weather.precipitation);
            ET0.Add(weatherData.weather.et0);
            VPD.Add(weatherData.weather.vpd);
            Rad.Add(weatherData.weather.radData.gsr);
            RHs.Add(weatherData.weather.relativeHumidity);
            LW.Add(weatherData.weather.leafWetness);
            WindSpeed.Add(weatherData.weather.windSpeed);

            if (weatherData.date.Hour == 23)
            {              
                float Tmax = (float)Temperatures.Max();
                float Tmin = (float)Temperatures.Min();
                float Prec = (float)Precipitation.Sum();
                float et0 = (float)ET0.Sum();
                float vpd = (float)VPD.Average();
                float lw = (float)LW.Sum();
                float windSpeed = (float)WindSpeed.Average();
                float rad = (float)Rad.Average();
                float rhMin = (float)RHs.Min();
                float rhMax = (float)RHs.Max();

                inputDaily inputDaily = new inputDaily();
                inputDaily.date = new DateTime(weatherData.date.Year, weatherData.date.Month, weatherData.date.Day);
                inputDaily.temperatureMax = Tmax;
                inputDaily.temperatureMin = Tmin;
                inputDaily.precipitation = Prec;
                inputDaily.et0 = et0;
                inputDaily.vpd = vpd;
                inputDaily.radiation = rad;
                inputDaily.relativeHumidityMax = rhMax;
                inputDaily.relativeHumidityMin = rhMin;
                inputDaily.leafWetness = lw; 
                inputDaily.windSpeed = windSpeed;


                //call the saetta model at each time step
                _Dormancy.runDormancy(inputDaily, parameters, output, outputT1);
                Temperatures = new List<double>();
                Precipitation = new List<double>(); 
                ET0 = new List<double>();
                VPD=new List<double>();
                LW = new List<double>();
                WindSpeed = new List<double>();
                Rad = new List<double>();
                RHs = new List<double>();

                //add weather data to output object
                outputT1.weatherInputDaily.date = inputDaily.date;
                outputT1.weatherInputDaily.temperatureMax = inputDaily.temperatureMax;
                outputT1.weatherInputDaily.temperatureMin = inputDaily.temperatureMin;
                outputT1.weatherInputDaily.precipitation = inputDaily.precipitation;
                outputT1.weatherInputDaily.radiation = inputDaily.radiation;
                outputT1.weatherInputDaily.et0 = inputDaily.et0;
                outputT1.weatherInputDaily.vpd = inputDaily.vpd;
                outputT1.weatherInputDaily.leafWetness = inputDaily.leafWetness;
                outputT1.weatherInputDaily.windSpeed = inputDaily.windSpeed;
                outputT1.weatherInputDaily.relativeHumidityMax = rhMax;
                outputT1.weatherInputDaily.relativeHumidityMin = rhMin;
            }
            else
            {
                outputT1.cropOutputs.chillState = output.cropOutputs.chillState;
                outputT1.cropOutputs.antiChillState = output.cropOutputs.antiChillState;
                outputT1.cropOutputs.isChillStarted = output.cropOutputs.isChillStarted;
            }

            //run forcing
            _Forcing.runForcing(weatherData, parameters, output, outputT1);

            if (!isCalibration)
            {
                //run transpiration
                _Transpiration.runTranspiration(weatherData, parameters, output, outputT1);
                //run phenomenals
                _Phenomenals.runPhenomenals(weatherData, parameters, output, outputT1);
            }
        }

        #region detailed bbch parameters
        private static phenologyParameters generateDetailedPhenologyParameters(phenologyParameters simpleParameters)
        {
            phenologyParameters detailedParameters = (phenologyParameters)simpleParameters;
            detailedParameters = new phenologyParameters();
            detailedParameters.phenomenalsParameters = simpleParameters.phenomenalsParameters;

            //if bbch99 is not present, include it
            if (!simpleParameters.BBCHParameters.ContainsKey(89))
            {
                simpleParameters.BBCHParameters.Add(89, new BBCH_Parameter());
                simpleParameters.BBCHParameters[89].cycleCompletion = 100;
            }

            List<int> keys = simpleParameters.BBCHParameters.Keys.AsEnumerable().ToList();
     
            for (int i = 0; i < keys.Count; i++)
            {
                //get current key
                int currentBbch = keys[i];
                int nextBbch = 0;
                //NOTE: assumption, simulations only before bbch 90
                if (currentBbch < 89)
                {
                    nextBbch = keys[i + 1];

                    int bbchGap = nextBbch - currentBbch;

                    for (int availableBbch = 0; availableBbch < bbchGap; availableBbch++)
                    {
                        int thisBbch = currentBbch + availableBbch;
                        detailedParameters.BBCHParameters.Add(thisBbch,new BBCH_Parameter());

                        BBCH_Parameter phenologyParameters = new BBCH_Parameter();
                        //compute the growing degree days at flowering

                        if(!simpleParameters.BBCHParameters.ContainsKey(65))
                        {
                            Console.WriteLine("something wrong");
                            Console.ReadLine();
                        }

                        float gddFlowering = simpleParameters.BBCHParameters[65].cycleCompletion / 100 *
                      simpleParameters.phenomenalsParameters.cycleLength;

                        float floweringFraction = simpleParameters.BBCHParameters[65].cycleCompletion/100;
                        if (nextBbch <= 65)
                        {
                            phenologyParameters.cycleCompletion =
                            linearInterpolator(currentBbch, simpleParameters.BBCHParameters[currentBbch].cycleCompletion,
                            nextBbch, simpleParameters.BBCHParameters[nextBbch].cycleCompletion, thisBbch)*
                            floweringFraction;

                        }
                       
                        else if (thisBbch == 65)
                        {
                            phenologyParameters.cycleCompletion = simpleParameters.BBCHParameters[65].cycleCompletion;
                        }
                        else if (nextBbch <= 89)
                        {
                            // Get the total cycle length and flowering completion
                            float cycleLength = simpleParameters.phenomenalsParameters.cycleLength;
                            float floweringCompletion = simpleParameters.BBCHParameters[65].cycleCompletion;
                            float gddFromFlowering = cycleLength * (1 - floweringCompletion / 100f); // GDD from BBCH65 to BBCH99

                            // Get the cycleCompletion at each BBCH phase (interpreted as % from flowering)
                            float startFromFlowering = simpleParameters.BBCHParameters[currentBbch].cycleCompletion;
                            float endFromFlowering = simpleParameters.BBCHParameters[nextBbch].cycleCompletion;

                            // Interpolate in terms of GDD from flowering
                            float interpolatedFromFlowering = 0;
                            if (currentBbch == 65)
                            {
                                interpolatedFromFlowering = linearInterpolator(currentBbch, 0, nextBbch, endFromFlowering, thisBbch);
                            }
                            else
                            {
                                interpolatedFromFlowering = linearInterpolator(currentBbch, simpleParameters.BBCHParameters[currentBbch].cycleCompletion,
                                    nextBbch, endFromFlowering, thisBbch); ;
                            }

                            // Scale to absolute GDD
                            float thisGdd = interpolatedFromFlowering / 100f * gddFromFlowering;

                            // Convert back to % of total cycle
                            float totalCompletion = (simpleParameters.BBCHParameters[65].cycleCompletion / 100f * 
                                cycleLength + thisGdd) / cycleLength * 100f;

                            phenologyParameters.cycleCompletion = totalCompletion;
                        }
                        
                        //add the istance to the dictionary
                        detailedParameters.BBCHParameters[thisBbch] = phenologyParameters;
                        if (thisBbch == 88)
                        {
                            detailedParameters.BBCHParameters.Add(89, new BBCH_Parameter());
                            detailedParameters.BBCHParameters[89].cycleCompletion = 100;
                            break;

                        }
                    }

                }

            }

          
            return detailedParameters;

        }

        public static float linearInterpolator(int currentBbch, float currentValue, int nextBbch, float nextValue, int thisBbch)
        {
            float interpolatedValue = 0;


            int x1 = currentBbch;
            float y1 = currentValue;
            int x2 = nextBbch;
            float y2 = nextValue;

            if ((x2 - x1) == 0)
            {
                interpolatedValue = (y2 + y1) / 2;
            }
            else
            {
                interpolatedValue = y1 + (thisBbch - x1) * (y2 - y1) / (x2 - x1);
            }


            return interpolatedValue;
        }
        #endregion
    }
}
