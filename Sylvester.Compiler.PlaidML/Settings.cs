using System;
using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json;

namespace Sylvester.Compiler.PlaidML
{
    public class Settings : CompilerApi<Settings>
    {
        public enum UseConfigFile
        {
            Default,
            Experimental,
            Environment,
            User
        }

        public static string CONFIG_VAR = "PLAIDML_CONFIG";
        public static string CONFIG_FILE_VAR = "PLAIDML_CONFIG_FILE";
        public static string DEVICE_IDS_VAR = "PLAIDML_DEVICE_IDS";
        public static string EXPERIMENTAL_VAR = "PLAIDML_EXPERIMENTAL";
        public static string SESSION_VAR = "PLAIDML_SESSION";
        public static string SETTINGS_VAR = "PLAIDML_SETTINGS";
        public static string TELEMETRY_VAR = "PLAIDML_TELEMETRY";

        public static string PLAIDML_EXPERIMENTAL_CONFIG_VAR = "PLAIDML_EXPERIMENTAL_CONFIG";
        public static string PLAIDML_DEFAULT_CONFIG_VAR = "PLAIDML_DEFAULT_CONFIG";

        public static string[] ENV_SETTINGS_VARS =
        {
            CONFIG_VAR, CONFIG_FILE_VAR, DEVICE_IDS_VAR,
            EXPERIMENTAL_VAR, SESSION_VAR, SETTINGS_VAR, TELEMETRY_VAR
        };

        public static FileInfo EnvironmentConfigFile =
            Environment.GetEnvironmentVariable(SETTINGS_VAR).IsNotNullOrEmpty()
                ? new FileInfo(Environment.GetEnvironmentVariable(SETTINGS_VAR))
                : null;

        public static FileInfo UserConfigFile =
            new FileInfo(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                ".plaidml"));

        public static FileInfo DefaultConfigFile =
            Environment.GetEnvironmentVariable(PLAIDML_DEFAULT_CONFIG_VAR).IsNotNullOrEmpty()
                ? new FileInfo(Environment.GetEnvironmentVariable(PLAIDML_DEFAULT_CONFIG_VAR))
                : new FileInfo(GetAssemblyDirectoryFullPath("config.json"));

        public static FileInfo ExperimentalConfigFile =
            Environment.GetEnvironmentVariable(PLAIDML_EXPERIMENTAL_CONFIG_VAR).IsNotNullOrEmpty()
                ? new FileInfo(Environment.GetEnvironmentVariable(PLAIDML_EXPERIMENTAL_CONFIG_VAR))
                : new FileInfo(GetAssemblyDirectoryFullPath("experimental.json"));

        public FileInfo ConfigFile { get; protected set; }

        public Dictionary<string, object> Dict { get; protected set; }

        public bool IsLoaded { get; protected set; }

        public string SessionId { get; protected set; }

        public bool SessionStarted { get; protected set; }

        public string ManualConfigText { get; protected set; }

        public string ConfigFileText { get; protected set; }

        public string Config
        {
            get
            {
                if (ManualConfigText.IsNotNullOrEmpty())
                {
                    return ManualConfigText;
                }
                else if (ConfigFileText.IsNotNullOrEmpty())
                {
                    return ConfigFileText;
                }
                else throw new InvalidOperationException("No configuration loaded.");
            }
        }

        public bool IsManualConfig => ManualConfigText.IsNotNullOrEmpty();

        static Settings()
        {
            if (Environment.GetEnvironmentVariable(PLAIDML_DEFAULT_CONFIG_VAR).IsNullOrEmpty())
            {
                Environment.SetEnvironmentVariable(PLAIDML_DEFAULT_CONFIG_VAR,
                    GetAssemblyDirectoryFullPath("config.json"));
            }

            if (Environment.GetEnvironmentVariable(PLAIDML_EXPERIMENTAL_CONFIG_VAR).IsNullOrEmpty())
            {
                Environment.SetEnvironmentVariable(PLAIDML_EXPERIMENTAL_CONFIG_VAR,
                    GetAssemblyDirectoryFullPath("experimental.json"));
            }
        }

        public Settings(string manualConfigText = "")
        {
            if (EnvironmentConfigFile != null && EnvironmentConfigFile.Exists)
            {
                ConfigFile = EnvironmentConfigFile;
                Info("Using PlaidML environment settings file {0}.", EnvironmentConfigFile.FullName);
            }
            else if (UserConfigFile.Exists)
            {
                ConfigFile = UserConfigFile;
                Info("Using PlaidML user settings file {0}.", UserConfigFile.FullName);
            }
            else
            {
                ConfigFile = DefaultConfigFile;
                Info("Using PlaidML default settings file {0}.", DefaultConfigFile.FullName);
            }

            ManualConfigText = manualConfigText;
            Load();
        }

        public Settings(UseConfigFile config)
        {
            if (config == UseConfigFile.Default)
            {
                ConfigFile = DefaultConfigFile;
                Info("Using PlaidML default settings file {0}.", ConfigFile.FullName);
            }
            else
            {
                ConfigFile = ExperimentalConfigFile;
                Info("Using PlaidML experimental settings file {0}.", ConfigFile.FullName);
            }

            Load();
        }

        public object this[string key]
        {
            get
            {
                ThrowIfNotLoaded();
                return Dict[key];
            }
        }


        public string StartNewSession()
        {
            ThrowIfNotLoaded();
            ThrowIfSessionStarted();
            SessionId = Guid.NewGuid().ToString("D");
            Environment.SetEnvironmentVariable(SESSION_VAR, SessionId);
            return SessionId;
        }

        protected bool Load()
        {
            try
            {
                var c = ConfigFile.OpenText().ReadToEnd();
                Dict = JsonConvert.DeserializeObject<Dictionary<string, object>>(c);
                if (Dict != null && Dict.Count > 0)
                {
                    Environment.SetEnvironmentVariable(CONFIG_FILE_VAR, ConfigFile.FullName);
                    ConfigFileText = c;
                    Info("Loaded configuration from file {0}.", ConfigFile.FullName);
                }

                IsLoaded = true;
            }
            catch (IOException ioe)
            {
                Error(ioe, "Error reading from file {0}.", ConfigFile.FullName);
            }
            catch (JsonSerializationException jse)
            {
                Error(jse, "Error deserializing configuration from file {0}.", ConfigFile.FullName);
                IsLoaded = false;
            }

            return IsLoaded;
        }

        protected void ThrowIfNotLoaded()
        {
            if (!IsLoaded)
            {
                throw new InvalidOperationException("Settings are not loaded.");
            }
        }

        protected void ThrowIfSessionStarted()
        {
            if (SessionStarted)
            {
                throw new InvalidOperationException("A session has already been started.");
            }
        }
    }
}