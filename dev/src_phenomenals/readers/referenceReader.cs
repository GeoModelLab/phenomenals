using System.Reflection;
using phenologyRunner.data;

namespace runner
{
    //read BBCH phases
    internal class referenceReader
    {
        public List<string> allVarieties = new List<string>();
        //this method reads the BBCH reference data from the .csv file
        internal Dictionary<string, simulationUnit> readBBCH(string site, string file, string variety)
        {
            Dictionary<string, simulationUnit> experiment_simUnit = new Dictionary<string, simulationUnit>();

            // Open the stream
            StreamReader sr = new StreamReader(file);
            // Read the first line (header)
            sr.ReadLine();

            // Loop over lines
            while (!sr.EndOfStream)
            {
                string[] line = sr.ReadLine().Split(',', '"');

                // Match variety or accept all if variety == "all"
                if (variety == "all" || variety == line[0] && site == line[1])
                {
                    var DateTime = Convert.ToDateTime(line[4]);

                    var dictKey = line[1] + "_" + line[0];

                    if (!experiment_simUnit.ContainsKey(dictKey))
                    {
                        experiment_simUnit.Add(dictKey, new simulationUnit());
                    }

                    if (!experiment_simUnit[dictKey].referenceData.year_BBCH_date.ContainsKey(DateTime.Year))
                    {
                        experiment_simUnit[dictKey].referenceData.year_BBCH_date.Add(DateTime.Year, new Dictionary<int, DateTime>());
                    }

                    experiment_simUnit[dictKey].referenceData.year_BBCH_date[DateTime.Year]
                        .Add(Convert.ToInt32(line[5]), DateTime);

                    experiment_simUnit[dictKey].variety = line[0];
                    experiment_simUnit[dictKey].latitude = float.Parse(line[2]);
                    experiment_simUnit[dictKey].longitude = float.Parse(line[3]);
                    experiment_simUnit[dictKey].site = line[1];

                    if (!allVarieties.Contains(line[0]))
                    {
                        allVarieties.Add(line[0]);
                    }
                }
            }

            sr.Close();

            return experiment_simUnit;
        }

    }
}
