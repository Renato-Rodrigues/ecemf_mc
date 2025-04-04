
import pyam

# to store your credentials, please run once
#    pyam.iiasa.set_config("login"", "password"")

def download_iiasa_meta_py(fileName, db, default_only = False):

    conn = pyam.iiasa.Connection(db)
    
    df = conn.properties(default_only=default_only)
    
    df.to_excel(fileName + ".xlsx")

def download_iiasa_db_py(fileName, db, model, scen, region, saveCSV = False):

    conn = pyam.iiasa.Connection(db)
    try:
	    df = conn.query(
            model=model,
            variable='*',
            region=region,
            scenario=scen
        )
	    if not (len(df) == 0):
	      df.to_excel(fileName + ".xlsx")
	      if saveCSV:
	        df.data.to_csv(fileName + ".csv", index = None, header=True)
    except:
      pass
