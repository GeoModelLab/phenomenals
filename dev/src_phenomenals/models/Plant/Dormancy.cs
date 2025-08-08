using phenologyRunner.dataStructure.Plant;

namespace phenologyRunner.models.Plant
{
    //Dormancy model - Cesaraccio C, Spano D, Snyder RL, Duce P (2004). Chilling and forcing model to predict bud-burst of crop and forest species. Agricultural and Forest Meteorology 126, 1:13.
    public class Dormancy
    {
        //run the dormancy model
        public void runDormancy(inputDaily Input, parameters Parameters, outputs Outputs, outputs Outputs1)
        {

            //compute average temperature
            float averageTemperature = (Input.temperatureMax + Input.temperatureMin) / 2;

            //compute chill days (first condition)
            if (Input.temperatureMin > Parameters.phenologyParameters.phenomenalsParameters.chillThreshold &&
                Input.temperatureMin >= 0)
            {
                Outputs1.cropOutputs.chillRate = 0;
                Outputs1.cropOutputs.antiChillRate = averageTemperature -
                    Parameters.phenologyParameters.phenomenalsParameters.chillThreshold;
            }
            //second condition
            else if (Input.temperatureMin >= 0 &&
                Input.temperatureMin <= Parameters.phenologyParameters.phenomenalsParameters.chillThreshold &&
                Input.temperatureMax > Parameters.phenologyParameters.phenomenalsParameters.chillThreshold)
            {
                float denominator = 2 * (Input.temperatureMax - Input.temperatureMin);

                Outputs1.cropOutputs.chillRate = -(averageTemperature - Input.temperatureMin -
                    (float)Math.Pow(Input.temperatureMax - Parameters.phenologyParameters.phenomenalsParameters.chillThreshold, 2) / denominator);

                Outputs1.cropOutputs.antiChillRate = (float)Math.Pow(Input.temperatureMax -
                    Parameters.phenologyParameters.phenomenalsParameters.chillThreshold, 2) / denominator;

                if (!Outputs1.cropOutputs.isChillStarted && Outputs.cropOutputs.chillRate < 0)
                {
                    Outputs1.cropOutputs.isChillStarted = true;
                }
            }
            //third condition
            else if (Input.temperatureMin >= 0 && 
                Input.temperatureMax <= Parameters.phenologyParameters.phenomenalsParameters.chillThreshold)
            {
                Outputs1.cropOutputs.chillRate = -(averageTemperature - Input.temperatureMin);
                Outputs1.cropOutputs.antiChillRate = 0;

                if (!Outputs1.cropOutputs.isChillStarted)
                {
                    Outputs1.cropOutputs.isChillStarted = true;
                }
            }
            //fourth condition
            else if (Input.temperatureMin < 0 && Input.temperatureMax >= 0 &&
                Input.temperatureMax <= Parameters.phenologyParameters.phenomenalsParameters.chillThreshold)
            {
                Outputs1.cropOutputs.chillRate = -(float)Math.Pow(Input.temperatureMax, 2) / (2 * (Input.temperatureMax - Input.temperatureMin));

                if (!Outputs1.cropOutputs.isChillStarted)
                {
                    Outputs1.cropOutputs.isChillStarted = true;
                }

                Outputs1.cropOutputs.antiChillRate = 0;
            }
            //fifth condition
            else if (Input.temperatureMin < 0 && 
                Input.temperatureMax > Parameters.phenologyParameters.phenomenalsParameters.chillThreshold)
            {
                float denominator = 2 * (Input.temperatureMax - Input.temperatureMin);

                Outputs1.cropOutputs.chillRate = -(float)Math.Pow(Input.temperatureMax, 2) /
                   denominator - (float)Math.Pow(Input.temperatureMax - Parameters.phenologyParameters.phenomenalsParameters.chillThreshold, 2) /
                    denominator;

                if (!Outputs1.cropOutputs.isChillStarted)
                {
                    Outputs1.cropOutputs.isChillStarted = true;
                }

                Outputs1.cropOutputs.antiChillRate = (float)Math.Pow(Input.temperatureMax - 
                    Parameters.phenologyParameters.phenomenalsParameters.chillThreshold, 2) /
                    denominator;
            }
            else
            {
                Outputs1.cropOutputs.antiChillRate = 0;
                Outputs1.cropOutputs.chillRate = 0;
            }

            //dormancy progression 
            if (Outputs.cropOutputs.chillState > -Parameters.phenologyParameters.phenomenalsParameters.chillingRequirement &&
                Outputs.cropOutputs.antiChillState == 0)
            {
                Outputs1.cropOutputs.chillState = Outputs.cropOutputs.chillState + Outputs1.cropOutputs.chillRate;
                Outputs1.cropOutputs.antiChillRate = 0;
                Outputs1.cropOutputs.antiChillState = 0;
                //compute dormancy completion percentage
                Outputs1.cropOutputs.dormancyCompletionPercentage = (Outputs1.cropOutputs.chillState /
                    -Parameters.phenologyParameters.phenomenalsParameters.chillingRequirement) / 2F * 100F;
            }
            else //dormancy release
            {
                Outputs1.cropOutputs.antiChillState = Outputs.cropOutputs.antiChillState + Outputs1.cropOutputs.antiChillRate;
                Outputs1.cropOutputs.chillState = Outputs.cropOutputs.chillState + Outputs1.cropOutputs.antiChillRate;
                Outputs1.cropOutputs.isChillStarted = true;

                //compute dormancy completion percentage
                Outputs1.cropOutputs.dormancyCompletionPercentage = 50 + (50- (Outputs1.cropOutputs.chillState /
                    -Parameters.phenologyParameters.phenomenalsParameters.chillingRequirement) / 2F * 100F);

                if (Outputs1.cropOutputs.chillState > 0)
                {
                    Outputs1.cropOutputs.chillState = 0;
                }
            }

        }
    }
}
