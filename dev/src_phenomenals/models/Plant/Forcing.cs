using phenologyRunner.dataStructure.Plant;

namespace phenologyRunner.models.Plant
{
    //Forcing model -  group (2022). A public decision support system for the assessment of plant disease infection risk shared by Italian regions. J Environ Manage 1:317:115365 https://www.sciencedirect.com/science/article/pii/S0301479722009380?via%3Dihub
    public class Forcing
    {
        //run the forcing model
        public void runForcing(inputPlant Input, parameters Parameters, outputs Outputs, outputs Outputs1)
        {
            //compute average temperature
            float hourlyTemperature = Input.weather.temperature;

            //the forcing rate is computed only if the chill state is greater than 0 and the chill is started
            if (Outputs1.cropOutputs.chillState >= 0 && Outputs1.cropOutputs.isChillStarted)
            {
                //compute forcing rate
                Outputs1.cropOutputs.forcingRate = (Parameters.phenologyParameters.phenomenalsParameters.ToptPhenology -
                    Parameters.phenologyParameters.phenomenalsParameters.TminPhenology) /24 *
                    temperature_function(hourlyTemperature, Parameters.phenologyParameters.phenomenalsParameters.TmaxPhenology,
                    Parameters.phenologyParameters.phenomenalsParameters.TminPhenology,
                    Parameters.phenologyParameters.phenomenalsParameters.ToptPhenology);
                Outputs1.cropOutputs.forcingState = Outputs.cropOutputs.forcingState + Outputs1.cropOutputs.forcingRate;
                //update dormancy at 100
                Outputs1.cropOutputs.dormancyCompletionPercentage = 100;
            }
            else
            {
                Outputs1.cropOutputs.forcingRate = 0;
                Outputs1.cropOutputs.forcingState = 0;
            }
            //compute cycle completion percentage
            Outputs1.cropOutputs.cycleCompletionPercentage = Outputs1.cropOutputs.forcingState /
                Parameters.phenologyParameters.phenomenalsParameters.cycleLength * 100;

            //check if the cycle is completed
            if (Outputs1.cropOutputs.cycleCompletionPercentage >= 100)
            {
                Outputs1.cropOutputs.cycleCompletionPercentage = 100;
                Outputs1.cropOutputs.forcingState = Parameters.phenologyParameters.phenomenalsParameters.cycleLength;
            }

            //compute BBCH code
            computeBBCH(Parameters, Outputs1);

            //update BBCH phenophase
            Outputs1.cropOutputs.bbchPhenophase = (int)Math.Round(Outputs1.cropOutputs.bbchPhenophaseCode,0);



        }

        #region private methods
        private void computeBBCH(parameters parameters, outputs Outputs)
        {
            //get all keys
            List<int> keys = parameters.phenologyParameters.BBCHParameters.Keys.AsEnumerable().ToList();


            float cumulatedGddThreshold = 0;
            //loop over them
            foreach (var _bbch in parameters.phenologyParameters.BBCHParameters.Keys)
            {

                //NOTE: assumption here
                if (_bbch <= 88)
                {
                    //this threshold 
                    cumulatedGddThreshold = parameters.phenologyParameters.BBCHParameters[_bbch].cycleCompletion / 100 *
                           parameters.phenologyParameters.phenomenalsParameters.cycleLength;
                    //next threshold
                    float nextGddThreshold = 
                        parameters.phenologyParameters.BBCHParameters[_bbch + 1].cycleCompletion / 100 *
                         parameters.phenologyParameters.phenomenalsParameters.cycleLength;

                    if (Outputs.cropOutputs.forcingState > cumulatedGddThreshold &&
                        Outputs.cropOutputs.forcingState < nextGddThreshold)
                    {
                        Outputs.cropOutputs.bbchPhenophaseCode = _bbch + (Outputs.cropOutputs.forcingState - cumulatedGddThreshold) /
                            (nextGddThreshold - cumulatedGddThreshold);
                        break;
                    }
                }
            }

        }


        public static float temperature_function(float Temperature, float Tmax, float Tmin, float Topt)
        {
            float Tfunction = 0;

            if (Temperature < Tmin || Temperature > Tmax)
            {
                Tfunction = 0;
            }
            else
            {
                float firstTerm = (Tmax - Temperature) /
                        (Tmax - Topt);
                float secondTerm = (Temperature - Tmin) /
                                     (Topt - Tmin);
                float Exponential = (Topt - Tmin) /
                                     (Tmax - Topt);

                Tfunction = firstTerm * (float)Math.Pow(secondTerm, Exponential);
            }
            return Tfunction;
        }

        #endregion
    }
}
