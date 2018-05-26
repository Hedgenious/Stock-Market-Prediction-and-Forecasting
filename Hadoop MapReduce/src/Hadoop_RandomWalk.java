/* Random Walk
given a particular stock, we’d like to know how often in the past several years its changed by 1%, 2%, 3% etc
(kind of like a a Fourier Transform, or transforming some temporal domain data into the frequency domain). This
is typical of the type of processing the MapReduce framework should be good at.

 */
import java.io.IOException;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;
import org.apache.hadoop.util.*;     
import java.text.DecimalFormat;

public class Hadoop_RandomWalk extends Configured implements Tool {

	public static class Map extends
			Mapper<LongWritable, Text, Text, IntWritable> {

		private final static IntWritable one = new IntWritable(1);
		private Text word = new Text();

		public void map(LongWritable key, Text value, Context context)
				throws IOException, InterruptedException {
			                                  
			// Date,Open,High,Low,Close,Volume,Adj Close
			// 2010-10-18,40.66,41.74,40.44,41.49,10620000,41.49   
			try {
				String[] tokens = value.toString().split(",");  
				System.out.println(value);
				System.out.println(tokens[1]);
				System.out.println(tokens[4]);

				float open = Float.valueOf(tokens[1]);
				float close = Float.valueOf(tokens[4]);
		   		float change = ((close - open)/open) * 100;   
		    	word.set(new DecimalFormat("0.##").format((double) change) + "%");
				context.write(word, one);    
			}   
			catch (NumberFormatException e) {
				// ignore first line
			}
		}
	}

	public static class Reduce extends
			Reducer<Text, IntWritable, Text, IntWritable> {
		public void reduce(Text key, Iterable<IntWritable> values,
				Context context) throws IOException, InterruptedException {

			int sum = 0;
			for (IntWritable val : values) {
				sum += val.get();
			}
			context.write(key, new IntWritable(sum));
		}
	}

	public int run(String[] args) throws Exception {
		Job job = new Job(getConf());
		job.setJarByClass(Hadoop_RandomWalk.class);
		job.setJobName("Hadoop_RandomWalk");

		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);

		job.setMapperClass(Map.class);
		job.setCombinerClass(Reduce.class);
		job.setReducerClass(Reduce.class);

		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.setInputPaths(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		boolean success = job.waitForCompletion(true);
		return success ? 0 : 1;
	}

	public static void main(String[] args) throws Exception {
		int ret = ToolRunner.run(new Hadoop_RandomWalk(), args);
		System.exit(ret);
	}
}
