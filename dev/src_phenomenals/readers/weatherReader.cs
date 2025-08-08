using phenologyRunner.dataStructure.Plant;

namespace runner
{
    public class weatherReader
    {
        public int startAvailableYear = 0;
        public int endAvailableYear=0;
        public Dictionary<DateTime, inputPlant> readHourly(string file, int startYear, int endYear, string site)
        {
            Dictionary<DateTime, inputPlant> gridWeathers = new Dictionary<DateTime, inputPlant>();
            Dictionary<DateTime, List<inputPlant>> dailyRecords = new Dictionary<DateTime, List<inputPlant>>();

            using (var sr = new StreamReader(new BufferedStream(new FileStream(file, FileMode.Open))))
            {
                string headerLine = sr.ReadLine();
                char delimiter = headerLine.Contains("\t") ? '\t' : ',';
                string[] headers = headerLine.Split(delimiter);

                // Normalize headers
                Dictionary<string, int> normalizedColumns = new Dictionary<string, int>();
                for (int i = 0; i < headers.Length; i++)
                {
                    string cleanName = CleanColumnName(headers[i]);
                    if (!normalizedColumns.ContainsKey(cleanName))
                        normalizedColumns.Add(cleanName, i);
                }

                // Aliases
                string[] siteAliases = { "site", "station", "location" };
                string[] dateAliases = { "date", "datetime", "timestamp" };
                string[] hourAliases = { "hour", "hr" };
                string[] tempAliases = { "temp", "temperature", "t2m" };
                string[] precAliases = { "prec", "precip", "precipitation", "prectotcorr", "rain", "rainfall" };
                string[] rhAliases = { "rh", "humidity", "relhumidity", "relativehumidity" };
                string[] windAliases = { "wind", "windspeed", "ws" };
                string[] radAliases = { "rad", "radiation", "solar", "solarrad" };
                string[] latAliases = { "latitude", "lat" };

                // Resolve indexes
                int siteIdx = GetColumnIndex(normalizedColumns, siteAliases);
                int dateIdx = GetColumnIndex(normalizedColumns, dateAliases);
                int hourIdx = GetColumnIndex(normalizedColumns, hourAliases);
                int tempIdx = GetColumnIndex(normalizedColumns, tempAliases);
                int precIdx = GetColumnIndex(normalizedColumns, precAliases);
                int rhIdx = GetColumnIndex(normalizedColumns, rhAliases);
                int windIdx = GetColumnIndex(normalizedColumns, windAliases);
                int radIdx = GetColumnIndex(normalizedColumns, radAliases);
                int latIdx = GetColumnIndex(normalizedColumns, latAliases);

                while (!sr.EndOfStream)
                {
                    string[] line = sr.ReadLine().Split(delimiter);
                    if (line.Length < normalizedColumns.Count) continue;

                    if (siteIdx != -1 && site != line[siteIdx]) continue;

                    DateTime timestamp = DateTime.Parse(line[dateIdx]).AddHours(Convert.ToDouble(line[hourIdx]));
                    if (timestamp.Year < startYear || timestamp.Year > endYear) continue;

                    inputPlant gw = new inputPlant();
                    gw.weather.date = timestamp;
                    gw.weather.temperature = ParseFloat(line, tempIdx);
                    gw.weather.precipitation = ParseFloat(line, precIdx);

                    float rhVal = 0f;
                    bool hasRH = rhIdx != -1 && float.TryParse(line[rhIdx], out rhVal);
                    gw.weather.relativeHumidity = rhVal;
                    if (!hasRH)
                    {
                        float t = gw.weather.temperature;
                        float dp = dewPoint(t, t);
                        float es = 0.61121f * (float)Math.Exp((17.502f * t) / (240.97F + t));
                        float ea = 0.61121f * (float)Math.Exp((17.502f * dp) / (240.97f + dp));
                        float estRH = ea / es * 100f;
                        gw.weather.relativeHumidity = Math.Min(estRH, 100);
                    }

                    gw.weather.windSpeed = GetOptionalValue(normalizedColumns, line, windAliases, 1.5f);

                    float latitude = latIdx != -1 ? ParseFloat(line, latIdx) : 0f;
                    gw.radData.latitude = latitude;

                    if (radIdx != -1 && float.TryParse(line[radIdx], out float radVal))
                    {
                        gw.weather.radiation = radVal;
                    }
                    else
                    {
                        gw.weather.radiation = 0f; // placeholder, will estimate later if needed
                    }

                    gw.weather.vpd = vpd(gw);
                    gw.weather.et0 = referenceEvapotranspiration(gw);
                    gw.weather.leafWetness = (gw.weather.relativeHumidity > 90 || gw.weather.precipitation > 0) ? 1 : 0;

                    DateTime dateOnly = timestamp.Date;
                    if (!dailyRecords.ContainsKey(dateOnly))
                        dailyRecords[dateOnly] = new List<inputPlant>();

                    dailyRecords[dateOnly].Add(gw);
                }
            }

            // Estimate missing radiation based on daily Tmax/Tmin
            foreach (var kvp in dailyRecords)
            {
                var date = kvp.Key;
                var records = kvp.Value;

                if (records.Count == 0) continue;

                float tmin = records.Min(r => r.weather.temperature);
                float tmax = records.Max(r => r.weather.temperature);
                float latitude = records.First().radData.latitude;

                inputPlant sample = records.First();
                sample.radData.latitude = latitude;

                radData estimatedRad = dayLength(sample, tmax, tmin);

                foreach (var record in records)
                {
                    int hour = record.weather.date.Hour;

                    if (record.weather.radiation <= 0)
                        record.weather.radiation = estimatedRad.gsrHourly[hour];

                    record.weather.radData = estimatedRad;

                    // Update dependent calculations
                    record.weather.vpd = vpd(record);
                    record.weather.et0 = referenceEvapotranspiration(record);
                    record.weather.leafWetness = (record.weather.relativeHumidity > 87 || record.weather.precipitation > 0) ? 1 : 0;

                    if (!gridWeathers.ContainsKey(record.weather.date))
                        gridWeathers.Add(record.weather.date, record);
                }
            }

            return gridWeathers;
        }

        public Dictionary<DateTime, inputPlant> readDaily(string file, int startYear, int endYear)
        {
            Dictionary<DateTime, inputPlant> gridWeathers = new Dictionary<DateTime, inputPlant>();
            
            using (var sr = new StreamReader(new BufferedStream(new FileStream(file, FileMode.Open))))
            {
                string headerLine = sr.ReadLine();
                char delimiter = headerLine.Contains("\t") ? '\t' : ','; // Auto-detect separator
                string[] headers = headerLine.Split(delimiter);

                // Clean and normalize headers
                Dictionary<string, int> normalizedColumns = new Dictionary<string, int>();
                for (int i = 0; i < headers.Length; i++)
                {
                    string cleanName = CleanColumnName(headers[i]);
                    if (!normalizedColumns.ContainsKey(cleanName))
                        normalizedColumns.Add(cleanName, i);
                }

                // Aliases
                string[] dateAliases = { "date", "datetime", "day" };
                string[] tmaxAliases = { "tmax", "t2mmax", "maxtemp" };
                string[] tminAliases = { "tmin", "t2mmin", "mintemp" };
                string[] precAliases = { "prec", "precip", "prectotcorr", "rain", "rainfall", "precipitation" };
                string[] windAliases = { "wind", "windspeed", "ws" };
                string[] rhmaxAliases = { "rhmax", "humiditymax", "relhumiditymax", "relativehumiditymax", "hummax" };
                string[] rhminAliases = { "rhmin", "humiditymin", "relhumiditymin", "relativehumiditymin", "hummin" };
                string[] radAliases = { "rad", "radiation", "solar", "solarrad" };
                string[] latAliases = { "latitude", "lat" };
               
                // Resolve required columns
                int dateIdx = GetColumnIndex(normalizedColumns, dateAliases);
                int tmaxIdx = GetColumnIndex(normalizedColumns, tmaxAliases);
                int tminIdx = GetColumnIndex(normalizedColumns, tminAliases);

                if (dateIdx == -1 || tmaxIdx == -1 || tminIdx == -1)
                    throw new Exception("Missing required column(s): date, tmax, or tmin");

                while (!sr.EndOfStream)
                {
                    string[] line = sr.ReadLine().Split(delimiter);
                    if (line.Length <= Math.Max(tmaxIdx, tminIdx)) continue; // Skip incomplete lines

                    inputDaily id = new inputDaily();

                    DateTime date = DateTime.Parse(line[dateIdx]);
                    id.date = date;

                    if (startAvailableYear == 0)
                        startAvailableYear = date.Year;

                    if (date.Year >= startYear && date.Year <= endYear)
                    {
                        id.temperatureMax = ParseFloat(line, tmaxIdx);
                        id.temperatureMin = ParseFloat(line, tminIdx);
                        id.precipitation = GetOptionalValue(normalizedColumns, line, precAliases);
                        id.windSpeed = GetOptionalValue(normalizedColumns, line, windAliases);
                        id.relativeHumidityMax = GetOptionalValue(normalizedColumns, line, rhmaxAliases);
                        id.relativeHumidityMin = GetOptionalValue(normalizedColumns, line, rhminAliases);
                        id.radiation = GetOptionalValue(normalizedColumns, line, radAliases);
                        id.radData.latitude = GetOptionalValue(normalizedColumns, line, latAliases);

                        id.dewPoint = dewPoint(id.temperatureMax, id.temperatureMin);

                        gridWeathers.AddRange(estimateHourly(id, date));
                    }
                }
            }

            return gridWeathers;
        }

        private string CleanColumnName(string name)
        {
            return name.Trim().ToLower()
                       .Replace("\u00A0", "") // Remove non-breaking space
                       .Replace("_", "")
                       .Replace("-", "")
                       .Replace(" ", "");
        }


        private int GetColumnIndex(Dictionary<string, int> columns, string[] aliases)
        {
            foreach (var alias in aliases)
            {
                string key = CleanColumnName(alias);
                if (columns.ContainsKey(key))
                    return columns[key];
            }
            return -1; // Not found
        }

        private float ParseFloat(string[] line, int index)
        {
            return (index < line.Length && float.TryParse(line[index], out float result))
                ? result
                : throw new Exception($"Missing or invalid required value at index {index}");
        }

        private float GetOptionalValue(Dictionary<string, int> columns, string[] line, string[] aliases, float defaultValue = 0f)
        {
            int idx = GetColumnIndex(columns, aliases);
            if (idx == -1 || idx >= line.Length || !float.TryParse(line[idx], out float result))
                return defaultValue;
            return result;
        }

        //method to calculate dewpoint t
        private float dewPoint(float tmax, float tmin)
        {
            return 0.38F * tmax - 0.018F *
                (float)Math.Pow(tmax, 2) + 1.4F * tmin - 5F;
        }

        //method to estimate hourly data
        public Dictionary<DateTime, inputPlant> estimateHourly(inputDaily inputDaily, DateTime date)
        {
            Dictionary<DateTime, inputPlant> dateTime_inputPlant = new Dictionary<DateTime, inputPlant>();

            float avgT = (inputDaily.temperatureMax + inputDaily.temperatureMin) / 2;
            float dailyRange = inputDaily.temperatureMax - inputDaily.temperatureMin;
            float dewPoint = Math.Clamp(inputDaily.dewPoint, inputDaily.temperatureMin - 5, inputDaily.temperatureMax);
            float rain = inputDaily.precipitation;

            bool hasRH = inputDaily.relativeHumidityMax > 0 && inputDaily.relativeHumidityMin > 0;
            bool hasRad = inputDaily.radiation > 0;
            bool hasWind = inputDaily.windSpeed > 0;

            for (int i = 0; i < 24; i++)
            {
                inputPlant inputPlant = new inputPlant();
                inputPlant.radData = inputDaily.radData;
                inputPlant.weather.date = date.AddHours(i);

                // Temperature Estimate
                float hourlyT = (float)(avgT + dailyRange / 2 * Math.Cos(0.2618f * (i - 15)));
                inputPlant.weather.temperature = hourlyT;

                // Relative Humidity Estimate
                if (hasRH)
                {
                    float rhRange = inputDaily.relativeHumidityMax - inputDaily.relativeHumidityMin;
                    inputPlant.weather.relativeHumidity = (float)(inputDaily.relativeHumidityMin +
                        rhRange / 2 * (1 - Math.Cos(2 * Math.PI * i / 24))); // peaks at night
                }
                else
                {
                    // fallback to dewpoint calculation
                    float es = 0.6108f * (float)Math.Exp((17.27f * hourlyT) / (237.3F + hourlyT));
                    float ea = 0.6108f * (float)Math.Exp((17.27F * dewPoint) / (237.3F + dewPoint));
                    float rh_hour = ea / es * 100;
                    inputPlant.weather.relativeHumidity = Math.Clamp(rh_hour, 0f, 100f);
                }

                // Precipitation
                inputPlant.weather.precipitation = (rain >= 0.2f) ? rain / 24 : 0f;

                // Leaf Wetness
                inputPlant.weather.leafWetness =
                    (inputPlant.weather.precipitation > 0 || inputPlant.weather.relativeHumidity > 90) ? 1 : 0;

                // Radiation
                if (hasRad)
                {
                    // evenly distribute or use sinusoidal pattern
                    inputPlant.radData.gsr = (float)(inputDaily.radiation); // day-peaking
                }

                inputPlant.radData = dayLength(inputPlant, inputDaily.temperatureMax, inputDaily.temperatureMin);
                inputPlant.weather.radiation = inputPlant.radData.gsrHourly[i];

                // Wind Speed
                if (hasWind)
                {
                    inputPlant.weather.windSpeed = inputDaily.windSpeed; // constant
                }
                else
                {
                    inputPlant.weather.windSpeed = 0f; // default fallback
                }

                // VPD
                inputPlant.weather.vpd = vpd(inputPlant);

                // GSR Total (just a copy for completeness)
                inputPlant.weather.radData.gsr = inputPlant.radData.gsr;

                // ET₀
                inputPlant.weather.et0 = referenceEvapotranspiration(inputPlant);

                dateTime_inputPlant.Add(inputPlant.weather.date, inputPlant);
            }

            return dateTime_inputPlant;
        }


        public radData dayLength(inputPlant input, float Tmax, float Tmin)
        {
            radData _radData = input.radData;

            // Constants
            float solarConstant = 4.921f; // MJ m⁻² h⁻¹ (derived from 1367 W/m²)
            float DtoR = (float)Math.PI / 180f;

            int doy = input.weather.date.DayOfYear;
            float latitudeRad = _radData.latitude * DtoR;

            // Solar geometry
            float inverseEarthSun = 1f + 0.0334f * (float)Math.Cos(0.01721f * doy - 0.0552f); // Earth-Sun distance correction
            float solarDeclination = 0.4093f * (float)Math.Sin((6.284f / 365f) * (284 + doy)); // radians
            float sinDec = (float)Math.Sin(solarDeclination);
            float cosDec = (float)Math.Cos(solarDeclination);
            float sinLat = (float)Math.Sin(latitudeRad);
            float cosLat = (float)Math.Cos(latitudeRad);
            float ss = sinDec * sinLat;
            float cc = cosDec * cosLat;
            float ws = (float)Math.Acos(-Math.Tan(solarDeclination) * Math.Tan(latitudeRad)); // sunset hour angle (radians)

            // Initialize arrays
            float[] HourAngleHourly = new float[24];
            float[] SolarElevationHourly = new float[24];
            float[] ExtraterrestrialRadiationHourly = new float[24];
            float[] Distribution = new float[24];
            _radData.etrHourly = new float[24];
            _radData.gsrHourly = new float[24];

            float dayHours = 0f;
            _radData.etr = 0f;

            // Loop through 24 hours to calculate hourly ETR
            for (int h = 0; h < 24; h++)
            {
                HourAngleHourly[h] = 15f * (h - 12f); // degrees
                float cosQHadj = ss + cc * (float)Math.Cos(DtoR * HourAngleHourly[h]);
                cosQHadj = Math.Clamp(cosQHadj, -1f, 1f);

                SolarElevationHourly[h] = (cosQHadj > 0f) ? (float)Math.Asin(cosQHadj) : 0f;

                float hourlyETR = solarConstant * inverseEarthSun * (float)Math.Sin(SolarElevationHourly[h]);
                hourlyETR = Math.Max(0f, hourlyETR); // no negative values

                _radData.etrHourly[h] = hourlyETR;
                _radData.etr += hourlyETR;

                if (hourlyETR > 0f) dayHours++;
            }

            // Compute analytical ETR and day length if within valid latitudes
            if (_radData.latitude < 65 && _radData.latitude > -65)
            {
                float dayLength = 0.13333f / DtoR * ws; // day length (hours)
                float Ra_analytical = (24f / (float)Math.PI) * solarConstant * inverseEarthSun *
                    (ws * ss + cc * (float)Math.Sin(ws));

                _radData.dayLength = dayLength;
                _radData.etr = Ra_analytical; // use analytical instead of loop? choose one
            }
            else
            {
                _radData.dayLength = dayHours;
            }

            // Estimate global solar radiation (GSR) if not measured
            if (_radData.gsr == 0f)
            {
                float kRs = 0.19f; // empirical constant
                float Rs = kRs * (float)Math.Sqrt(Tmax - Tmin) * _radData.etr;
                _radData.gsr = (float)Math.Round(Rs, 2); // MJ m⁻² day⁻¹
            }

            // Redistribute GSR over 24 hours using ETR fractions
            for (int h = 0; h < 24; h++)
            {
                Distribution[h] = (_radData.etr > 0f) ? _radData.etrHourly[h] / _radData.etr : 0f;
                _radData.gsrHourly[h] = Distribution[h] * _radData.gsr;
            }

            // Estimate sunrise and sunset
            _radData.hourSunrise = 12f - _radData.dayLength / 2f;
            _radData.hourSunset = 12f + _radData.dayLength / 2f;

            return _radData;
        }


        public float referenceEvapotranspiration(inputPlant Input)
        {
            // Convert Rs from W/m² to MJ/m²/h
            double Rs_MJ = Input.weather.radiation;

            // Given coefficients
            double c0 = 0.1396;
            double c1 = -3.019e-3;
            double c2 = -1.2109e-3;
            double c3 = 1.626e-5;
            double c4 = 8.224e-5;
            double c5 = 0.1842;
            double c6 = -1.095e-3;
            double c7 = 3.655e-3;
            double c8 = -4.442e-3;

            // Compute ET0 using the equation
            double ET0 = c0
                         + c1 * Input.weather.relativeHumidity
                         + c2 * Input.weather.temperature
                         + c3 * Math.Pow(Input.weather.relativeHumidity, 2)
                         + c4 * Math.Pow(Input.weather.temperature, 2)
                         + c5 * Rs_MJ
                         + 0.5 * Rs_MJ * (c6 * Input.weather.relativeHumidity + c7 * Input.weather.temperature)
                         + c8 * Math.Pow(Rs_MJ, 2);

            if (ET0 < 0) { ET0 = 0; };
            return (float)ET0;

        }

        public float latentHeatVaporization(float Tmax, float Tmin)
        {
            float latentHeat = 0;
            float a = 2.501F;    // intercept
            float b = 0.002361F; // slope

            float avgT = 0.5F * (Tmax + Tmin);
            latentHeat = a - b * avgT;

            return latentHeat;
        }

        public float vpd(inputPlant Input)
        {
            //VPD calculation method by Monteith and Unsworth (1990)

            float SVP = 0.6108f * (float)Math.Exp((17.27f * Input.weather.temperature) / (Input.weather.temperature + 237.3f)); // in kPa
            float AVP = SVP * Input.weather.relativeHumidity / 100f;
            float VPD = SVP - AVP;
            return VPD;
        }

    }


    public static class DictionaryExtensions
    {
        public static void AddRange<TKey, TValue>(this Dictionary<TKey, TValue> target, Dictionary<TKey, TValue> source)
        {
            foreach (var kvp in source)
            {
                // You can choose to overwrite or skip existing keys
                target[kvp.Key] = kvp.Value; // Overwrites if key exists
                                             // OR use TryAdd (C# 9+) to skip existing keys:
                                             // target.TryAdd(kvp.Key, kvp.Value);
            }
        }
    }
}
