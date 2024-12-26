#include <stdio.h>
#include <stdlib.h>
#include <math.h>
typedef struct Point_s
{
    double x;
    double y;
}Point;

// Function to calculate the dot product of two vectors
double dot_product(Point a, Point b) {
    return a.x * b.x + a.y * b.y;
}

// Function to calculate the magnitude of a vector
double magnitude(Point a) {
    return sqrt(a.x * a.x + a.y * a.y);
}

// Function to calculate the angle in degrees between two vectors
double calculate_angle(Point a, Point b, Point c) {
    // Create vectors AB and BC
    Point ab = { b.x - a.x, b.y - a.y };
    Point bc = { b.x - c.x, b.y - c.y };

    // Calculate the dot product of AB and BC
    double dot = dot_product(ab, bc);

    // Calculate the magnitude of vector AB and vector BC
    double magAB = magnitude(ab);
    double magBC = magnitude(bc);

    // Calculate the angle in radians between AB and BC
    double angleRadians = acos(dot / (magAB * magBC));

    // Convert the angle to degrees
    double angleDegrees = angleRadians * (180.0 / M_PI);

    return angleDegrees;
}

int main()
{
    int amountOfCalls;
    for(int step = 0; step < amountOfCalls; step++)
    {
        Point A, B, C;
        scanf("%lf %lf %lf %lf %lf %lf", &A.x, &A.y, &B.x, &B.y, &C.x, &C.y);
        
    }
}