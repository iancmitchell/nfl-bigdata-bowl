import pandas as pd # type: ignore
import os 
from typing import List, Tuple
import numpy as np # type: ignore
from pathlib import Path
import logging 
import sys
import json
from pandarallel import pandarallel

pandarallel.initialize(nb_workers=12)

WEEKS =  (1,18)
CURRENT_DIRECTORY = Path(__file__).resolve().parent
DATA_LOCATION = CURRENT_DIRECTORY / "../" / "data" 
EXPORT_LOCATION = DATA_LOCATION / "fields"

class GenerateFields(object):
    def __init__(self, weeks: Tuple[int, int] = WEEKS, data_location: Path = DATA_LOCATION, export_location: Path = EXPORT_LOCATION):
        self.weeks = weeks
        self.data_location = data_location
        self.export_location = export_location
        self.field_dims = (0,0)
        log_handler = logging.StreamHandler(sys.stdout)
        logging.basicConfig(level=logging.INFO, format='[%(asctime)s] {%(filename)s:%(lineno)d} %(levelname)s - %(message)s', handlers=[log_handler])
        self.logger = logging.getLogger("GenerateFields")

    def generate_field(self, row: pd.Series) -> np.array:
        field = np.zeros((self.field_dims[0],self.field_dims[1])) 
        for idx, x in enumerate(row["x"]):
            y = row["y"][idx]
            try:
                field[x-1][y-1] = 1
            except Exception as e:
                print(e)
                print(row)
                print(f"X: {x} | Y: {y}")
        return json.dumps(field.tolist())

    def import_week(self, week_number: int) -> pd.DataFrame:
        self.logger.info(f"Importing Week: {week_number}")
        week = pd.read_csv(f"{DATA_LOCATION}/week{week_number}.csv")
        week.loc[:,"x"] = week["x"] * 10
        week.loc[:,"y"] = week["y"] * 10
        week.loc[:,"x"] = week["x"].astype("int")
        week.loc[:,"y"] = week["y"].astype("int")
        return week

    def process_week(self, week_number: int, field_dims: Tuple[int, int]):
        self.logger.info(f"Processing Week: {week_number}")
        week = self.import_week(week_number)
        tracking_data_cols = ["gameId", "playId", "frameId", "x", "y", "time"]
        week = week[tracking_data_cols]
        week = week.groupby(["gameId","playId","frameId"]).agg(list)
        week["time"] = week["time"].apply(lambda x: min(x))
        week["field"] = week.parallel_apply(self.generate_field, axis=1)
        self.logger.info(f"Fields Generated For Week: {week_number}")
        self.export_week(week, week_number)

    def export_week(self, week: pd.DataFrame, week_number: int):
        self.logger.info(f"Exporting Week: {week_number}")
        if not os.path.exists(self.export_location):
            os.makedirs(self.export_location)    
        week.to_csv(f"{self.export_location}/week{week_number}.csv")

    def get_field_size(self, week_numbers: range) -> Tuple[int, int]:
        max_x = 0
        max_y = 0
        for week_number in week_numbers:
            week = self.import_week(week_number)   
            x = week["x"].max()
            y = week["y"].max()
            self.logger.info(f"Week: {week_number} | X: {x} | Y: {y}")
            max_x = max([x, max_x])
            max_y = max([y, max_y])
            del week
        self.field_dims = (max_x, max_y)
        self.logger.info(f"Field Size: {self.field_dims}")
        return self.field_dims

    def process(self):
        week_numbers = range(self.weeks[0], self.weeks[1])
        self.logger.info(f"Processing Weeks: {self.weeks[0]} - {self.weeks[1]}")
        field_dims = self.get_field_size(week_numbers)
        for week in week_numbers:
            self.process_week(week, field_dims)

def main():
    client = GenerateFields()
    client.process()

if __name__ == "__main__":
    main()