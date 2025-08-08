using phenologyRunner.dataStructure.Plant;

namespace phenologyRunner.models.Plant
{
    //Stress functions 

    public class Phenomenals
    {
        //run the phenomenals
       
        public void runPhenomenals(inputPlant Input, parameters Parameters, outputs Outputs, outputs Outputs1)
        {

            //water stress effect
            Outputs1.cropOutputs.aridityFunction = 1-aridityFunction(Input, Parameters, Outputs1);
            Outputs1.cropOutputs.aridityPhenomenal = Outputs.cropOutputs.aridityPhenomenal + Outputs1.cropOutputs.aridityFunction;

            //temperature effect
            Outputs1.cropOutputs.temperatureFunction = temperaturePhysiology(Input, Parameters.phenologyParameters.phenomenalsParameters.TminPhysiology,
                Parameters.phenologyParameters.phenomenalsParameters.ToptPhysiology, Parameters.phenologyParameters.phenomenalsParameters.TmaxPhysiology);
            Outputs1.cropOutputs.tempPhenomenal = Outputs.cropOutputs.tempPhenomenal + Outputs1.cropOutputs.temperatureFunction;

            //cold stress effect
            Outputs1.cropOutputs.coldStressFunction = coldStressFunction(Input, Parameters);
            Outputs1.cropOutputs.coldPhenomenal = Outputs.cropOutputs.coldPhenomenal + Outputs1.cropOutputs.coldStressFunction;

            //heat stress effect
            Outputs1.cropOutputs.heatStressFunction = heatStressFunction(Input, Parameters);
            Outputs1.cropOutputs.heatPhenomenal = Outputs.cropOutputs.heatPhenomenal + Outputs1.cropOutputs.heatStressFunction;

            //light effect
            Outputs1.cropOutputs.lightFunction = lightfunction(Input.weather.radiation,Parameters);
            Outputs1.cropOutputs.lightPhenomenal = Outputs.cropOutputs.lightPhenomenal + Outputs1.cropOutputs.lightFunction;

            //VPD effect
            Outputs1.cropOutputs.VPDeficitFunction = VPDfunction(Input.weather.vpd, Parameters);
            Outputs1.cropOutputs.vpdPhenomenal = Outputs.cropOutputs.vpdPhenomenal + Outputs1.cropOutputs.VPDeficitFunction;

            //wind effect
            Outputs1.cropOutputs.windFunction = windFunction(Input.weather.windSpeed, Parameters);
            Outputs1.cropOutputs.windPhenomenal = Outputs.cropOutputs.windPhenomenal + Outputs1.cropOutputs.windFunction;

            //disease effect
            Outputs1.cropOutputs.diseaseFunction = diseaseFunction(Input, Parameters);
            Outputs1.cropOutputs.disPhenomenal = Outputs.cropOutputs.disPhenomenal + Outputs1.cropOutputs.diseaseFunction;

            Outputs1.cropOutputs.temperatureFunctionState = Outputs.cropOutputs.temperatureFunctionState;
            Outputs1.cropOutputs.aridityFunctionState = Outputs.cropOutputs.aridityFunctionState;
            Outputs1.cropOutputs.coldStressFunctionState = Outputs.cropOutputs.coldStressFunctionState;
            Outputs1.cropOutputs.heatStressFunctionState = Outputs.cropOutputs.heatStressFunctionState;
            Outputs1.cropOutputs.lightFunctionState = Outputs.cropOutputs.lightFunctionState;
            Outputs1.cropOutputs.VPDeficitFunctionState = Outputs.cropOutputs.VPDeficitFunctionState;
            Outputs1.cropOutputs.windFunctionState = Outputs.cropOutputs.windFunctionState;
            Outputs1.cropOutputs.diseaseFunctionState = Outputs.cropOutputs.diseaseFunctionState;

            if (Input.weather.date.Hour == 23)
            {
                float tempPhenomenalAverage = Outputs1.cropOutputs.tempPhenomenal / 24;
                float heatPhenomenalAverage = Outputs1.cropOutputs.heatPhenomenal / 24;
                float coldPhenomenalAverage = Outputs1.cropOutputs.coldPhenomenal / 24;
                float lightPhenomenalAverage = Outputs1.cropOutputs.lightPhenomenal / 24;
                float aridityPhenomenalAverage = Outputs1.cropOutputs.aridityPhenomenal / 24;
                float vpdPhenomenalAverage = Outputs1.cropOutputs.vpdPhenomenal / 24;
                float windPhenomenalAverage = Outputs1.cropOutputs.windPhenomenal / 24;
                float diseasePhenomenalAverage = Outputs1.cropOutputs.disPhenomenal / 24;

                Outputs1.cropOutputs.temperatureFunctionState = Outputs.cropOutputs.temperatureFunctionState + tempPhenomenalAverage;
                Outputs1.cropOutputs.aridityFunctionState = Outputs.cropOutputs.aridityFunctionState + aridityPhenomenalAverage;
                Outputs1.cropOutputs.coldStressFunctionState = Outputs.cropOutputs.coldStressFunctionState + coldPhenomenalAverage;
                Outputs1.cropOutputs.heatStressFunctionState = Outputs.cropOutputs.heatStressFunctionState + heatPhenomenalAverage;
                Outputs1.cropOutputs.lightFunctionState = Outputs.cropOutputs.lightFunctionState + lightPhenomenalAverage;
                Outputs1.cropOutputs.VPDeficitFunctionState = Outputs.cropOutputs.VPDeficitFunctionState + vpdPhenomenalAverage;
                Outputs1.cropOutputs.windFunctionState = Outputs.cropOutputs.windFunctionState + windPhenomenalAverage;
                Outputs1.cropOutputs.diseaseFunctionState = Outputs.cropOutputs.diseaseFunctionState + diseasePhenomenalAverage;

                Outputs1.cropOutputs.tempPhenomenal = 0;
                Outputs1.cropOutputs.heatPhenomenal = 0;
                Outputs1.cropOutputs.coldPhenomenal = 0;
                Outputs1.cropOutputs.lightPhenomenal = 0;
                Outputs1.cropOutputs.aridityPhenomenal = 0;
                Outputs1.cropOutputs.vpdPhenomenal = 0;
                Outputs1.cropOutputs.windPhenomenal = 0;
                Outputs1.cropOutputs.disPhenomenal = 0;
            }

        }

        #region private methods
        public float temperaturePhysiology(inputPlant Input, float Tmin, float Topt, float Tmax)
        {
            float Tfunction = 0;

            float Temperature = Input.weather.temperature;

            if (Temperature < Tmin || Temperature > Tmax)
            {
                Tfunction = 0;
            }                        
            else
            {
                float firstTerm = (Tmax - Temperature) / (Tmax - Topt);
                float secondTerm = (Temperature - Tmin) /(Topt - Tmin);
                float Exponential = (Topt - Tmin) /(Tmax - Topt);

                Tfunction = firstTerm * (float)Math.Pow(secondTerm, Exponential);                
            }
            return Tfunction;
        }
        public float heatStressFunction(inputPlant Input, parameters Parameters)
        {
            float heatStress = 0;

            if (Input.weather.temperature < Parameters.phenologyParameters.phenomenalsParameters.TmaxPhysiology)
            {
                heatStress = 1;
            }
            else if (Input.weather.temperature >= Parameters.phenologyParameters.phenomenalsParameters.temperatureHeatCritical)
            {
                heatStress = 0;
            }
            else
            {
                //compute lag and slope
                double midpoint = (Parameters.phenologyParameters.phenomenalsParameters.TmaxPhysiology +
                     Parameters.phenologyParameters.phenomenalsParameters.temperatureHeatCritical) / 2;
                double width = Math.Abs(Parameters.phenologyParameters.phenomenalsParameters.TmaxPhysiology -
                   Parameters.phenologyParameters.phenomenalsParameters.temperatureHeatCritical);

                //update chilling units
                heatStress = 1 / (1 + (float)Math.Exp(10 / width * ((Input.weather.temperature - midpoint))));
            }
            return heatStress;
        }
        public float coldStressFunction(inputPlant Input, parameters Parameters)
        {
            float coldStress = 0;

            if (Input.weather.temperature <= Parameters.phenologyParameters.phenomenalsParameters.temperatureColdCritical)
            {
                coldStress = 0;
            }
            else if (Input.weather.temperature > Parameters.phenologyParameters.phenomenalsParameters.TminPhysiology)
            {
                coldStress = 1;
            }
            else
            {
                //compute lag and slope
                double midpoint = (Parameters.phenologyParameters.phenomenalsParameters.TminPhysiology +
                    Parameters.phenologyParameters.phenomenalsParameters.temperatureColdCritical) / 2;
                double width = Math.Abs(Parameters.phenologyParameters.phenomenalsParameters.TminPhysiology -
                   Parameters.phenologyParameters.phenomenalsParameters.temperatureColdCritical);

                //update chilling units
                coldStress = 1 / (1 + (float)Math.Exp(10 / -width * ((Input.weather.temperature - midpoint))));
            }

            return coldStress;
        }
        public float aridityFunction(inputPlant Input, parameters Parameters, outputs Outputs1)
        {
            float aridity = 0;
            //fixed coefficiet for grass
            float et0 = Input.weather.et0 * Outputs1.cropOutputs.cropCoefficient;

            Outputs1.cropOutputs.PrecipitationMemory.Add(Input.weather.precipitation);
            Outputs1.cropOutputs.ET0memory.Add(et0);

            if (Outputs1.cropOutputs.PrecipitationMemory.Count < Parameters.phenologyParameters.phenomenalsParameters.waterStressDays * 24)
            {
                aridity = 1;
            }
            else
            {
                //compute water stress: https://doi.org/10.1016/j.geoderma.2021.115003
                float sumPrecipitation = Outputs1.cropOutputs.PrecipitationMemory.Sum();
                float sumET0 = Outputs1.cropOutputs.ET0memory.Sum();

                aridity = ((sumET0-sumPrecipitation)/(sumPrecipitation+sumET0)+1)/2;

                //remove first day
                Outputs1.cropOutputs.PrecipitationMemory.RemoveAt(0);
                Outputs1.cropOutputs.ET0memory.RemoveAt(0);

            }

            //set maximum water stress to 0
            if (aridity < 0 || float.IsNaN(aridity))
            {
                aridity = 0;
            }

            return aridity;
        }
        public float lightfunction(float radiation, parameters parameters)
        {
            float lightResponse = parameters.phenologyParameters.phenomenalsParameters.lightSensitivity;
            float lightFunction = 1 - (float)Math.Exp(-lightResponse * (radiation*0.5)/ parameters.phenologyParameters.phenomenalsParameters.lightMax) ;

            return lightFunction;
        }
        public float VPDfunction(float vpd, parameters parameters)
        {
            float vpdMin = parameters.phenologyParameters.phenomenalsParameters.VPDmin;
            float vpdMax = parameters.phenologyParameters.phenomenalsParameters.VPDmax;
            float kVPD = parameters.phenologyParameters.phenomenalsParameters.VPDsensitivity;

            if (vpd < vpdMin)
            {
                return 1f;
            }
            else
            {
                float midpoint = (vpdMax + vpdMin) / 2f;
                float exponent = kVPD * (vpd - midpoint);
                float result = 1f / (1f + (float)Math.Exp(exponent));
                return result;
            }
        }

        public float windFunction(float windSpeed, parameters parameters)
        {
            float windMin = parameters.phenologyParameters.phenomenalsParameters.windMin;
            float kWind = parameters.phenologyParameters.phenomenalsParameters.windSensitivity;

            if (windSpeed < windMin)
            {
                return 1f;
            }
            else
            {
                float result = (float)Math.Exp(-kWind * (windSpeed - windMin));
                return result;
            }
        }


        List<float> temperaturePathogen = new List<float>();
        public float diseaseFunction(inputPlant Input, parameters Parameters)
        {
            float pathogenFunction = 0;
            if(Input.weather.leafWetness == 1 && 
                (Input.weather.temperature > Parameters.phenologyParameters.phenomenalsParameters.TminDisease &&
                Input.weather.temperature < Parameters.phenologyParameters.phenomenalsParameters.TmaxDisease))
            {
                temperaturePathogen.Add(temperaturePhysiology(Input,
                        Parameters.phenologyParameters.phenomenalsParameters.TminDisease,
                        Parameters.phenologyParameters.phenomenalsParameters.ToptDisease,
                        Parameters.phenologyParameters.phenomenalsParameters.TmaxDisease));
            }


            if (Input.weather.date.Hour == 23)
            {
                float wetHours = temperaturePathogen.Count;

                if (wetHours < Parameters.phenologyParameters.phenomenalsParameters.wetnessMin)
                {
                    pathogenFunction = 0;
                }
                else if (wetHours <= Parameters.phenologyParameters.phenomenalsParameters.wetnessMax)
                {
                    float tFunction = temperaturePathogen.Average();
                    //temperature function

                    //effectiveness of wetness hours
                    float wetnessEffectiveness = tFunction * (float)temperaturePathogen.Count();
                    pathogenFunction = wetnessEffectiveness / Parameters.phenologyParameters.phenomenalsParameters.wetnessMax;
                }
                else
                {
                   
                        pathogenFunction = 1;
                   
                }
                temperaturePathogen = new List<float>();
            }

            return pathogenFunction;
        }

        #endregion
    }
}