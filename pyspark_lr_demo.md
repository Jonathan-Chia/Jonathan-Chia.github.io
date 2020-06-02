

Below is a logistic regression model I deployed in pyspark on housing data.

Key takeaways include:

NOTE: use the output files from your lreg.out to fill in some of the outputs in the below code


from pyspark.sql import SparkSession

# Build the SparkSession
spark = SparkSession.builder \
   .master("spark://10.5.91.201:7077") \
   .appName("Linear Regression Model") \
   .config("spark.executor.memory", "1gb") \
   .getOrCreate()
   
sc = spark.sparkContext


#export SPARK_HOME="/usr/local/spark/"

rdd = sc.textFile('/user/jchia/cadataclean.txt')

rdd = rdd.map(lambda line: line.split(","))

# Import the necessary modules
from pyspark.sql import Row

# Map the RDD to a DF
df = rdd.map(lambda line: Row(
                              longitude=line[7],
                              latitude=line[8],
                              housingMedianAge=line[2],
                              totalRooms=line[3],
                              totalBedRooms=line[4],
                              population=line[5],
                              households=line[6],
                              medianIncome=line[1],
                              medianHouseValue=line[0])).toDF()

df.show()

df.columns
df.printSchema()
# Import all from `sql.types`
from pyspark.sql.types import *

# Write a custom function to convert the data type of DataFrame columns
def convertColumn(df, names, newType):
  for name in names: 
     df = df.withColumn(name, df[name].cast(newType))
  return df 

# Assign all column names to `columns`
columns = ['households', 'housingMedianAge', 'latitude', 'longitude', 'medianHouseValue', 'medianIncome', 'population', 'totalBedRooms', 'totalRooms']

# Conver the `df` columns to `FloatType()`
df = convertColumn(df, columns, FloatType())

df.printSchema()

####Data exploration

df.select('population','totalBedRooms').show(10)

df.groupBy("housingMedianAge").count().sort("housingMedianAge",ascending=False).show()

df.describe.show()

# Import all from `sql.functions` 
from pyspark.sql.functions import *

# Adjust the values of `medianHouseValue`
df = df.withColumn("medianHouseValue", col("medianHouseValue")/100000)

# Show the first 2 lines of `df`
df.take(2)

# Import all from `sql.functions` if you haven't yet
from pyspark.sql.functions import *

# Divide `totalRooms` by `households`
roomsPerHousehold = df.select(col("totalRooms")/col("households"))

# Divide `population` by `households`
populationPerHousehold = df.select(col("population")/col("households"))

# Divide `totalBedRooms` by `totalRooms`
bedroomsPerRoom = df.select(col("totalBedRooms")/col("totalRooms"))

# Add the new columns to `df`
df = df.withColumn("roomsPerHousehold", col("totalRooms")/col("households")) \
   .withColumn("populationPerHousehold", col("population")/col("households")) \
   .withColumn("bedroomsPerRoom", col("totalBedRooms")/col("totalRooms"))
   
# Inspect the result
df.first()

df = df.select("medianHouseValue", 
              "totalBedRooms", 
              "population", 
              "households", 
              "medianIncome", 
              "roomsPerHousehold", 
              "populationPerHousehold", 
              "bedroomsPerRoom")

# Import `DenseVector`
from pyspark.ml.linalg import DenseVector

# Define the `input_data` 
input_data = df.rdd.map(lambda x: (x[0], DenseVector(x[1:])))

# Replace `df` with the new DataFrame
df = spark.createDataFrame(input_data, ["label", "features"])

# Import `StandardScaler` 
from pyspark.ml.feature import StandardScaler

# Initialize the `standardScaler`
standardScaler = StandardScaler(inputCol="features", outputCol="features_scaled")

# Fit the DataFrame to the scaler
scaler = standardScaler.fit(df)

# Transform the data in `df` with the scaler
scaled_df = scaler.transform(df)

# Inspect the result
scaled_df.take(2)

# Split the data into train and test sets
train_data, test_data = scaled_df.randomSplit([.8,.2],seed=1234)

# Import `LinearRegression`
from pyspark.ml.regression import LinearRegression

# Initialize `lr`
lr = LinearRegression(labelCol="label", maxIter=10, regParam=0.3, elasticNetParam=0.8)

# Fit the data to the model
linearModel = lr.fit(train_data)

# Generate predictions
predicted = linearModel.transform(test_data)

# Extract the predictions and the "known" correct labels
predictions = predicted.select("prediction").rdd.map(lambda x: x[0])
labels = predicted.select("label").rdd.map(lambda x: x[0])

# Zip `predictions` and `labels` into a list
predictionAndLabel = predictions.zip(labels).collect()

# Print out first 5 instances of `predictionAndLabel` 
predictionAndLabel[:5]

# Coefficients for the model
linearModel.coefficients

# Intercept for the model
linearModel.intercept


# Get the RMSE
linearModel.summary.rootMeanSquaredError

# Get the R2
linearModel.summary.r2

spark.stop()
