using phenologyRunner.data;

namespace runner
{
    public class paramReader
    {
        public  Dictionary<string, parameter> read(string file)
        {
            Dictionary<string, parameter> nameParam = new Dictionary<string, parameter>();

           
            StreamReader sr = new StreamReader(file);
            sr.ReadLine();

            while(!sr.EndOfStream)
            {
                string[] line = sr.ReadLine().Split(',');


                nameParam.Add(line[1]+"_"+line[2], new parameter());
                parameter parameter = new parameter();
                parameter.value = float.Parse(line[5]);
                parameter.minimum = float.Parse(line[3]);
                parameter.maximum = float.Parse(line[4]);
                parameter.calibration = line[6];
                parameter.paramClass = line[2];
                nameParam[line[1] + "_" + line[2]] = parameter;
                              
            }
            sr.Close();
            return nameParam;
        }

        public Dictionary<string, float> calibratedRead(string file)
        {
            var paramCalibValue = new Dictionary<string, float>();

            if (File.Exists(file))
            {
                StreamReader sr = new StreamReader(file);
                sr.ReadLine();

                while (!sr.EndOfStream)
                {
                    string[] line = sr.ReadLine().Split(',');
                    paramCalibValue.Add(line[0], float.Parse(line[1]));
                }
                sr.Close();
            }
            return paramCalibValue;
        }
    }


    public class parameter
    {
        public float minimum { get; set; }
        public float maximum { get; set; }
        public float value { get; set; }
        public string calibration { get; set; }
        public string paramClass { get; set; }
    }
}
