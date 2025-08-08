using phenologyRunner.dataStructure.Plant;

namespace phenologyRunner.models.Plant
{
    //Stress functions 

    public class Transpiration
    {
        //run transpiration
        public void runTranspiration(inputPlant Input, parameters Parameters, outputs Outputs, outputs Outputs1)
        {
            //no transpiration before bud break
            
                //estimate crop coefficient
                float bbch = Outputs1.cropOutputs.bbchPhenophaseCode;
                float kc;

                // FAO-based Kc assignment based on BBCH
                if (bbch <= 10)
                {
                    kc = 0.10F + (Parameters.phenologyParameters.phenomenalsParameters.kcInitial - 0.10F) * (bbch / 10.0F);
                }
                else if (bbch <= 50)
                {
                    kc = Parameters.phenologyParameters.phenomenalsParameters.kcInitial +
                         (Parameters.phenologyParameters.phenomenalsParameters.kcFullCanopy -
                          Parameters.phenologyParameters.phenomenalsParameters.kcInitial) *
                         ((bbch - 10.0F) / 40.0F);  // 50 - 10 = 40
                }
                else if (bbch <= 65)
                {
                    kc = Parameters.phenologyParameters.phenomenalsParameters.kcFullCanopy;
                }
                else if (bbch <= 89)
                {
                    kc = Parameters.phenologyParameters.phenomenalsParameters.kcFullCanopy -
                         (Parameters.phenologyParameters.phenomenalsParameters.kcFullCanopy -
                          Parameters.phenologyParameters.phenomenalsParameters.kcLateSeason) *
                         ((bbch - 65.0F) / 24.0F);  // 89 - 65 = 24
                }
                else
                {
                    kc = Parameters.phenologyParameters.phenomenalsParameters.kcLateSeason;
                }

                // Apply result
                Outputs1.cropOutputs.cropCoefficient = kc;


                //estimate transpiration (mm h-1)
                Outputs1.cropOutputs.cropTranspiration = Input.weather.et0 * Outputs1.cropOutputs.cropCoefficient;

            

        }
    }
}