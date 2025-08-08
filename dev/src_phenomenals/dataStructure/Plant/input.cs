namespace phenologyRunner.dataStructure.Plant
{
    public class inputPlant
    {
        public DateTime date { get; set; } //date
        public crop_parameter_class parameters = new crop_parameter_class();

        #region Weather data
        public inputWeather weather = new inputWeather();
        #endregion

        public location location = new location();

        public radData radData = new radData();

        public string variety { get; set; }
    }

    public class inputWeather
    {
        public DateTime date { get; set; }
        public float dayLength { get; set; }
        public float relativeHumidity { get; set; }
        public float et0 { get; set; }
        public float temperature { get; set; }
        public float precipitation { get; set; }
        public float leafWetness { get; set; }
        public float radiation { get; set; }
        public float windSpeed { get; set; }
        public float vpd { get; set; }


        public radData radData = new radData();
    }

    public class inputDaily : inputWeather
    {
        public float relativeHumidityMax { get; set; }
        public float relativeHumidityMin { get; set; }
        public float temperatureMax { get; set; }
        public float temperatureMin { get; set; }
        public float dewPoint { get; set; }
    }

    public class crop_parameter_class
    {
        public Dictionary<string, Dictionary<string, float>> crop_parameter_value = new Dictionary<string, Dictionary<string, float>>();

    }

    public class location
    {
        public float latitude { get; set; }
        public float longitude { get; set; }
        public string gridName { get; set; }
    }

    public class radData
    {
        public float latitude { get; set; } //latitude
        public float etr { get; set; } //extraterrestrial solar radiation, MJ m-2 d-1 (not strictly needed)
        public float dayLength { get; set; } //hours
        public float dayLengthYesterday { get; set; } //hours
        public float hourSunrise { get; set; } //hour
        public float hourSunset { get; set; } //hour
        public float gsr { get; set; } //global solar radiation, MJ m-2 d-1

        public float[] etrHourly = new float[24];
        public float[] gsrHourly = new float[24];

    }


}
