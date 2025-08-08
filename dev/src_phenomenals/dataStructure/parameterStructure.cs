namespace phenologyRunner.dataStructure
{
    //this class define a parameter: the corresponding class, the minimum, maximum values, and the inclusion of the parameter in the calibration subset
    public class parameterStructure
    {
        public string classParam { get; set; }
        public float minimum { get; set; }
        public float maximum { get; set; }
        public float value { get; set; }
        public string calibration { get; set; }
    }
}
