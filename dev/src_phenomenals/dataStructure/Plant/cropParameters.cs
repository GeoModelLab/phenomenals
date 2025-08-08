namespace phenologyRunner.dataStructure.Plant
{
    public class parameters
    {
        public phenologyParameters phenologyParameters = new phenologyParameters();

    }

    #region the phenomenals
    public class phenologyParameters
    {

        public phenomenalsParameters phenomenalsParameters = new phenomenalsParameters();

        public Dictionary<int, BBCH_Parameter> BBCHParameters = new Dictionary<int, BBCH_Parameter>();

    }

    #region classes

    public class phenomenalsParameters
    {
        //NOTE: this names must be present in the cropParameters file
        public float TminPhenology { get; set; } // °C
        public float ToptPhenology { get; set; } // °C
        public float TmaxPhenology { get; set; } // °C
        public float cycleLength { get; set; } // °C-days
        public float chillThreshold { get; set; }//chilling hours
        public float chillingRequirement { get; set; } //chilling hours
        public float temperatureHeatCritical { get; set; } // °C
        public float temperatureColdCritical { get; set; } // °C
        public float waterStressDays { get; set; } //days
        public float kcInitial { get; set; } //unitless
        public float kcFullCanopy { get; set; } //unitless
        public float kcLateSeason { get; set; } //unitless
        public float lightSensitivity { get; set; } //unitless
        public float lightMax { get; set; } //unitless
        public float VPDmin { get; set; } //kPa
        public float VPDmax { get; set; } //kPa
        public float VPDsensitivity { get; set; } //kPa

        public float windSensitivity { get; set; }//unitless
        public float windMin { get; set; }//mm s-1
        public float TminDisease { get; set; }// °C
        public float ToptDisease { get; set; }// °C
        public float TmaxDisease { get; set; }// °C
        public float wetnessMin { get; set; }//hours
        public float wetnessMax { get; set; } //hours
        public float TminPhysiology { get; set; } // °C
        public float ToptPhysiology { get; set; } // °C
        public float TmaxPhysiology { get; set; } // °C
    }

    public class BBCH_Parameter
    {
        public float cycleCompletion { get; set; }
    }

    #endregion

    #endregion


}
