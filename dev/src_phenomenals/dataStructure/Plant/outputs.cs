namespace phenologyRunner.dataStructure.Plant
{
    public class outputs
    {
        public string currentCrop { get; set; }
        public string currentVariety { get; set; }
        public string timestep { get; set; }

        public summaryOutputs summaryOutputs = new summaryOutputs();

        public cropOutputs cropOutputs = new cropOutputs();

        public inputPlant weatherInputHourly = new inputPlant();

        public inputDaily weatherInputDaily = new inputDaily();

    }
    public class cropOutputs
    {
        // get or set the chill days.
        public float chillState { get; set; }
        public float chillRate { get; set; }
        public float antiChillRate { get; set; }
        public float antiChillState { get; set; }
        public float forcingRate { get; set; }
        public float forcingState { get; set; }
        public float bbchPhenophaseCode { get; set; }
        public float cycleCompletionPercentage { get; set; }
        public float dormancyCompletionPercentage { get; set; }
        public float phaseCompletionPercentage { get; set; }
        public int bbchPhenophase { get; set; }
        public int bbchReproductivePhase { get; set; }
        public int bbchVegetativePhase { get; set; }
        public bool isChillStarted { get; set; }
        public float cropCoefficient { get; set; } //unitless 0-1, 1 no stress
        public float cropTranspiration { get; set; } //unitless 0-1, 1 no stress

        #region phenomenals
        public float heatStressFunction { get; set; } //unitless 0-1, 1 no stress
        public float heatStressFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float coldStressFunction { get; set; } //unitless 0-1, 1 no stress
        public float coldStressFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float aridityFunction { get; set; } //unitless 0-1, 1 no stress
        public float aridityFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float VPDeficitFunction { get; set; } //unitless 0-1, 1 no stress
        public float VPDeficitFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float lightFunction{ get; set; } //unitless 0-1, 1 no stress
        public float lightFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float temperatureFunction { get; set; } //unitless 0-1, 1 no stress
        public float temperatureFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float windFunction { get; set; } //unitless 0-1, 1 no stress
        public float windFunctionState { get; set; } //unitless 0-1, 1 no stress
        public float diseaseFunction{ get; set; } //unitless 0-1, 1 no stress
        public float diseaseFunctionState { get; set; } //unitless 0-1, 1 no stress

        public float tempPhenomenal { get; set; }
        public float coldPhenomenal { get; set; }
        public float heatPhenomenal { get; set; }
        public float lightPhenomenal { get; set; }
        public float vpdPhenomenal { get; set; }
        public float windPhenomenal { get; set; }
        public float aridityPhenomenal { get; set; }
        public float disPhenomenal { get; set; }

        #endregion

        #region auxiliary

        public bool isFlowering { get; set; } // boolean

        public Dictionary<int, Dictionary<int, DateTime>> yearBbchDate = new Dictionary<int, Dictionary<int, DateTime>>();

        public List<float> PrecipitationMemory = new List<float>(); //to compute water stress
        public List<float> ET0memory = new List<float>(); //to compute water stress
        #endregion
    }

    public class summaryOutputs
    {
        public string currentCrop { get; set; }
        public string currentVariety { get; set; }

        public Dictionary<int, DateTime> bbchDate = new Dictionary<int, DateTime>();
    }
}
