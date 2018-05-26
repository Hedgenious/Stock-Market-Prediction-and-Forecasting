import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

//Student Class

class Student
{
    String name;

    int marks;

    public Student(String name, int marks)
    {
        this.name = name;

        this.marks = marks;
    }
}

//nameCompare Class to compare the names

class nameCompare implements Comparator<Student>
{
    @Override
    public int compare(Student s1, Student s2)
    {
        return s1.name.compareTo(s2.name);
    }
}

//marksCompare Class to compare the marks

class marksCompare implements Comparator<Student>
{
    @Override
    public int compare(Student s1, Student s2)
    {
        return s2.marks - s1.marks;
    }
}

