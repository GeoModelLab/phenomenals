
namespace phenologyRunner.data
{
    //this class define a pixel: the corresponding class, the minimum, maximum values, and the inclusion of the parameter in the calibration subset
    public class simulationUnit
    {
        public string site { get; set; }
        public float latitude { get; set; }
        public float longitude { get; set; }
        public string variety { get; set; }

        public referenceData referenceData = new referenceData();
    }

    public class referenceData
    {
        public Dictionary<int, Dictionary<int, DateTime>> year_BBCH_date = new Dictionary<int, Dictionary<int, DateTime>>();
        public Dictionary<int, DateTime> year_DM_onset_date = new Dictionary<int, DateTime>();
        public Dictionary<int, DateTime> year_DM_firstInfection_date = new Dictionary<int, DateTime>();
        public Dictionary<int, Dictionary<int, float>> year_DM_dynamics = new Dictionary<int, Dictionary<int, float>>();
    }
}
