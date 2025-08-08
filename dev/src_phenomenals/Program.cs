using runner;
using System.Text.Json;
using UNIMI.optimizer;
using phenologyRunner.data;
using phenologyRunner.dataStructure.Plant;
using phenologyRunner.Utilities;
using System.Globalization;
using System.Text;

CultureInfo.DefaultThreadCurrentCulture = CultureInfo.InvariantCulture;
CultureInfo.DefaultThreadCurrentUICulture = CultureInfo.InvariantCulture;
Console.OutputEncoding = Encoding.UTF8;

#region read the configuration file (agrarsenseConfig.json)
//Console.WriteLine("ARGS:");
//foreach (var a in args)
//    Console.WriteLine(a);

// Define an array of bright colors
ConsoleColor[] colors = new ConsoleColor[]
{
    ConsoleColor.Cyan,
    ConsoleColor.Yellow,
    ConsoleColor.Green,
    ConsoleColor.Magenta,
    ConsoleColor.Blue
};


if (args.Length == 0 || !File.Exists(args[0]))
{
    Console.WriteLine("❌ Config path missing or invalid.");
    return;
}
string configPath = args[0];
//Console.WriteLine("✅ Using config file: " + configPath);

string jsonString = File.ReadAllText(configPath);
var config = JsonSerializer.Deserialize<Root>(jsonString);

#endregion

#region settings
//switch between calibration and validation
bool isCalibration = bool.Parse(config.settings.isCalibration);
//Console.WriteLine("CALIBRATION MODE: {0}", isCalibration);
int startYear = config.settings.startYear.GetValueOrDefault();
int endYear = config.settings.endYear.GetValueOrDefault();
//Console.WriteLine("START YEAR: {0}", startYear);
//Console.WriteLine("END YEAR: {0}", endYear);
List<string> sites = config.settings.sites;
//Console.WriteLine("SITES: {0}", sites[0]);
int simplexes = config.settings.simplexes.GetValueOrDefault();
int iterations = config.settings.iterations.GetValueOrDefault();
//Console.WriteLine("SIMPLEXES: {0}", simplexes);
//Console.WriteLine("ITERATIONS: {0}", iterations);
string weatherTimeStep = config.settings.weatherTimeStep;
//Console.WriteLine("WEATHER TS: {0}", weatherTimeStep);


string runningMode = "";
if (isCalibration) runningMode = "calibration"; else runningMode = "validation";

//Console.WriteLine("RUNNING MODE: {0}", runningMode);

//set species
List<string> varieties = config.settings.varieties;
// Print all varieties as a comma-separated string
//Console.WriteLine("VARIETIES: {0}", string.Join(", ", varieties));
//Console.WriteLine("");
//Console.WriteLine("If you want to change settings, please close this prompt and edit the json configuration file.\nhit return to start...");
//Console.WriteLine("Phenology calibration starting");

#endregion

#region paths
//set paths
string weatherDir = config.paths.weatherDir;
string plantParamFile = config.paths.plantParamFile;
#endregion

//optimizer class
var optimizer = new runner.optimizer();

//read crop parameter file with ranges for automatic calibration
var paramReader = new runner.paramReader();
var nameParamPlant = paramReader.read(plantParamFile);
var nameParam = nameParamPlant.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
optimizer.nameParam = nameParam;

//data structure to store calibrated parameters
Dictionary<string, float> paramCalibValue = new Dictionary<string, float>();

// Get the directory where the executable is running
string exeDir = AppDomain.CurrentDomain.BaseDirectory;
Random rnd = new Random();
Console.ForegroundColor = colors[rnd.Next(colors.Length)]; // Pick a random color

#region switch between calibration and validation
if (isCalibration)
{
    #region calibration

    #region define optimizer settings
    //optimizer instance
    MultiStartSimplex msx = new MultiStartSimplex();

    //number of simplexes of the automatic optimizer
    msx.NofSimplexes = simplexes;
    //tolerance of the automatic optimizer
    msx.Ftol = 0.001;
    //maximum number of iterations for each simplex
    msx.Itmax = iterations;
    #endregion

    for (int site = 0; site < sites.Count; site ++)
    {
        var thisSite = sites[site];
        var referenceReader = new runner.referenceReader();

        if (config.settings.varieties.Contains("all"))
        {
            // Se "all", leggi tutte le varietà disponibili per il sito
            referenceReader.readBBCH(sites[site], config.paths.referenceFileBBCH, "all");
            varieties = referenceReader.allVarieties;
        }
        else
        {
            // Se varietà specifiche, non leggere ora — lo fai nel ciclo
            varieties = config.settings.varieties;
        }

        for (int variety = 0; variety < varieties.Count; variety++)
        {

            #region read reference data
            optimizer.site_simulationUnit = referenceReader.readBBCH(sites[site], 
                config.paths.referenceFileBBCH, varieties[variety]);
            #endregion

            //simulations are executed if there are any sites for this variety
            if (optimizer.site_simulationUnit.Count > 0)
            {
                optimizer.availableSites = sites;

                ////message to console
                Console.WriteLine("\ncalibration running on {0}, {1}....", varieties[variety], sites[site]);

                //set grid cell
                #region define parameters settings for calibration
                //count parameters under calibration
                int paramCalibrated = 0;

                Dictionary<string, float> param_outCalibration = new Dictionary<string, float>();

                //loop over parameters
                foreach (var name in optimizer.nameParam.Keys)
                {
                    //add parameter to calibration if calibration field is not empty (x)
                    if (optimizer.nameParam[name].calibration != "") { paramCalibrated++; }

                }
                //set number of dimension in the matrix
                double[,] Limits = new double[paramCalibrated, 2];

                //populate limits 
                //count parameters under calibration
                int i = 0;
                foreach (var name in optimizer.nameParam.Keys)
                {
                    if (optimizer.nameParam[name].calibration != "")
                    {
                        Limits[i, 1] = optimizer.nameParam[name].maximum;
                        Limits[i, 0] = optimizer.nameParam[name].minimum;
                        i++;
                    }
                    else
                    {
                        param_outCalibration.Add(name, optimizer.nameParam[name].value);
                    }
                }
                double[,] results = new double[1, 1];
                #endregion

                #region set optimizer calibration properties
                optimizer.isCalibration = isCalibration;
                optimizer.param_outCalibration = param_outCalibration;
                optimizer.startYear = startYear;
                optimizer.endYear = endYear;
                optimizer.weatherDir = weatherDir;
                optimizer.variety = varieties[variety];
                optimizer.plantParamFile = plantParamFile;
                optimizer.weatherTimeStep = weatherTimeStep;
                optimizer.consoleColor = colors[rnd.Next(colors.Length)];
                optimizer.iterations = iterations;
                #endregion

                //run optimizer
                msx.Multistart(optimizer, paramCalibrated, Limits, out results);

                //get calibrated parameters
                paramCalibValue = new Dictionary<string, float>();
                int count = 0;

                #region write calibrated parameters
                string header = "param, value";
                List<string> writeParam = new List<string>();
                writeParam.Add(header);
                foreach (var param in optimizer.nameParam.Keys)
                {
                    if (optimizer.nameParam[param].calibration != "")
                    {
                        //write a line for each parameter
                        string line = "";
                        line += param + ",";
                        line += results[0, count];
                        writeParam.Add(line);
                        //assign the calibrated value to the parameter
                        paramCalibValue.Add(param, (float)results[0, count]);
                        count++;
                    }
                }

               
                // Build full path to calibratedVarieties folder
                string outputDir = Path.Combine(exeDir, "calibratedVarieties");

                // Ensure the folder exists
                Directory.CreateDirectory(outputDir);

                // Build the file path
                string fileName = $"parameters_{sites[site]}_{varieties[variety]}.csv";
                string fullPath = Path.Combine(outputDir, fileName);

                // Write the file
                File.WriteAllLines(fullPath, writeParam);

                #endregion

                //empty dictionary of dates and outputs objects
                var dateOutputs = new Dictionary<DateTime, outputs>();
                //execute model with calibrated parameters
                optimizer.oneShot(paramCalibValue, out dateOutputs);

                //Console.WriteLine(" BEST");
            }
            else
            {
                Console.WriteLine("No experiment available for variety {0} in site {1}", 
                    varieties[variety], sites[site]);
            }

        }      
    }
  
    #endregion
}
else
{
    #region validation
    //read calibrated files
    string outputDir = Path.Combine(exeDir, "calibratedVarieties");
    if (!Directory.Exists(outputDir))
    {
        Console.WriteLine($"Directory not found: {outputDir}");
        return;
    }
    FileInfo[] calibratedVarieties = new DirectoryInfo(outputDir).GetFiles();

    foreach (var site in sites)
    {
        var referenceReader = new runner.referenceReader();

        var thisSite = site;
        if (config.settings.varieties.Contains("all"))
        {
            // Se "all", leggi tutte le varietà disponibili per il sito
            referenceReader.readBBCH(thisSite, config.paths.referenceFileBBCH, "all");
            varieties = referenceReader.allVarieties;
        }
        else
        {
            // Se varietà specifiche, non leggere ora — lo fai nel ciclo
            varieties = config.settings.varieties;
        }

        foreach (var variety in varieties)
        {
            // Check if any file name in calibratedVarieties contains the variety name
            bool varietyFound = calibratedVarieties.Any(file => file.Name.Contains(site + "_" + variety , 
                StringComparison.OrdinalIgnoreCase));

            // Check if the file name contains the variety name
            if (varietyFound) // Case insensitive check
            {
                //create a simulation unit
                var simulationUnit = new simulationUnit();

                //read reference data
                #region read reference data
                optimizer.site_simulationUnit = referenceReader.readBBCH(thisSite, 
                    config.paths.referenceFileBBCH, variety);
                #endregion

                //message to console
                Console.WriteLine("simulation running on {0}, {1}....", variety, site);

                #region read calibrated parameters
                string plantFile = outputDir + $"//parameters_{site}_{variety}.csv";

                // Read files or stop if they don't exist
                var paramCalibValuePlant = utilities.ReadFileOrExitsParameters(plantFile, "Plant", variety);

                // Combine dictionaries
                paramCalibValue = paramCalibValuePlant.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);


                #endregion
                if (paramCalibValue.Count > 0)
                {

                    #region assign default parameters (out of calibration subset)
                    //parameters out of calibration
                    Dictionary<string, float> param_outCalibration = new Dictionary<string, float>();
                    optimizer.nameParam = nameParam;
                    //populate parameters out of calibration
                    foreach (var name in optimizer.nameParam.Keys)
                    {
                        if (optimizer.nameParam[name].calibration == "")
                        {
                            //add parameter to calibration if calibration field is not empty (x)
                            param_outCalibration.Add(name, optimizer.nameParam[name].value);
                        }
                    }
                    #endregion

                    #region set optimizer validation properties
                    //set optimizer properties
                    optimizer.availableSites = sites;
                    optimizer.isCalibration = isCalibration;
                    optimizer.variety = variety;
                    optimizer.weatherDir = weatherDir;
                    //pass parameters
                    optimizer.nameParam = nameParam;
                    optimizer.param_outCalibration = param_outCalibration;
                    //set start and end year
                    optimizer.startYear = startYear;
                    optimizer.endYear = endYear;
                    optimizer.weatherTimeStep = weatherTimeStep;
                    #endregion

                    var dateOutputs = new Dictionary<DateTime, outputs>();

                    //run in validation (from optimizer class)
                    optimizer.oneShot(paramCalibValue, out dateOutputs);                   
                }
            }

        }
    }

    #endregion
}
#endregion

public class Root
{
    public Settings? settings { get; set; }
    public Paths? paths { get; set; }

}

//contains the parameters in the json configuration file
public class Settings
{

    public string? isCalibration { get; set; }
    public List<string>? varieties { get; set; }
    public int? startYear { get; set; }
    public int? endYear { get; set; }
    public List<string>? sites { get; set; }
    public int? simplexes { get; set; }
    public int? iterations { get; set; }
    public string weatherTimeStep { get; set; }

}

public class Paths
{
    public string? weatherDir { get; set; }
    public string? plantParamFile { get; set; }
    public string? outputDir { get; set; }
    public string? referenceFileBBCH { get; set; }
}