using phenologyRunner.data;
using phenologyRunner.dataStructure.Plant;
using runner;

namespace phenologyRunner.Utilities
{
    public class utilities
    {
		public static double temperature_function(double Temperature, double Tmax,
		   double Tmin, double Topt)
		{
			double Tfunction = 0;

			if (Temperature < Tmin || Temperature > Tmax)
			{
				Tfunction = 0;
			}
			else
			{
				double firstTerm = (Tmax - Temperature) /
						(Tmax - Topt);
				double secondTerm = ((Temperature - Tmin) /
									 (Topt - Tmin));
				double Exponential = (Topt - Tmin) /
									 (Tmax - Topt);

				Tfunction = firstTerm * Math.Pow(secondTerm, Exponential);
			}
			return Tfunction;
		}

       

        // Dedicated method to check file existence and read, or stop program if missing
        public static Dictionary<string, float> ReadFileOrExitsParameters(string filePath, string fileType, string variety)
        {
            Dictionary<string, float> ReadFileOrExit = new Dictionary<string, float>();
            if (!File.Exists(filePath))
            {
                Console.WriteLine($"Error: {fileType} file not found for variety '{variety}' at {filePath}.");
                //Console.WriteLine("Press Enter to exit...");
                // Console.ReadLine(); // Wait for user input
                // Environment.Exit(0); // Exit the program
            }
           

                paramReader _paramReader = new paramReader();
            ReadFileOrExit = _paramReader.calibratedRead(filePath); 
            return ReadFileOrExit; // Read and return the file content
        }
    }
}
