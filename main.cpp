// Galia Shabanova CS-01
#include <iostream>
#include <bits/stdc++.h>


using namespace std;

class Matrix {
public:
    int rows;
    int columns;
    double **arr2;

    Matrix(int r, int c) {
        this->rows = r;
        this->columns = c;
        arr2 = new double*[rows];

        for (int i = 0; i < rows; ++i)
            arr2[i] = new double[columns];
    }


    Matrix* operator =(Matrix &mat1) {

        for (int i = 0; i < mat1.rows; i++) {
            for (int j = 0; j < mat1.columns; j++) {
                mat1.arr2[i][j] = arr2[i][j];

            }
        }
        return &mat1;
    }

    Matrix * operator +(Matrix *mat1) {
        Matrix *sum = new Matrix(mat1->rows, mat1->columns);
        if ((mat1->rows == rows) && (mat1->columns == columns)) {
            for (int i = 0; i < mat1->rows; i++) {
                for (int j = 0; j < mat1->columns; j++) {
                    sum->arr2[i][j] = mat1->arr2[i][j] + arr2[i][j];
                }
            }
            return sum;
        }
        return nullptr;
    }

    Matrix * operator -(Matrix *mat1) {
        Matrix *dif = new Matrix(mat1->rows, mat1->columns);
        if ((mat1->rows == rows) && (mat1->columns == columns)) {
            for (int i = 0; i < mat1->rows; i++) {
                for (int j = 0; j < mat1->columns; j++) {
                    dif->arr2[i][j] = arr2[i][j] - mat1->arr2[i][j];
                }
            }
        } else {
            return nullptr;
        }
        return dif;
    }

    Matrix * operator *(Matrix *mat1) {
        Matrix *product = new Matrix(rows, mat1->columns);
        if (mat1->rows == columns) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < mat1->columns; j++) {
                    double temp = 0;
                    for (int k = 0; k < columns; k++) {
                        temp += arr2[i][k] * mat1->arr2[k][j];
                    }
                    product->arr2[i][j] = temp;
                }
            }
        } else {
            return nullptr;
        }
        return product;
    }

    friend istream & operator>>(istream &in, Matrix &inputMatrix) {
        for (int i = 0; i < inputMatrix.rows; i++)
            for (int j = 0; j < inputMatrix.columns; j++)
                in >> inputMatrix.arr2[i][j];
        return in;
    }
    friend ostream & operator<<(ostream &out, Matrix &outMatrix) {
        for (int i = 0; i < outMatrix.rows; i++) {
            for (int j = 0; j < outMatrix.columns; j++) {

                out << fixed << setprecision(4) << outMatrix.arr2[i][j] << " ";
            }
            out << endl;
        }


        return out;
    }

    Matrix * T() {
        Matrix *transposed = new Matrix(columns, rows);
        for (int i = 0; i < rows; i++) {
            for (int j  = 0; j < columns; j++) {
                transposed->arr2[j][i] = arr2[i][j];

            }
        }
        return transposed;
    }

};

class SquareMatrix : public Matrix{
public:

    explicit SquareMatrix(int r) : Matrix(r, r) {}

    bool operator==(SquareMatrix m) {
        bool res = true;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                if (arr2[i][j] != m.arr2[i][j])
                    res = false;
            }
        }
        return res;
    }
};

class IdentityMatrix : public SquareMatrix {
public:
    explicit IdentityMatrix(int r) : SquareMatrix(r) {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                if (i == j)
                    arr2[i][j] = 1;
                else
                    arr2[i][j] = 0;
            }
        }
    }
};

class EliminationMatrix : public IdentityMatrix {
public:
    int posI;
    int posJ;
    double coefficient;
    EliminationMatrix(int r, double coefficient, int posI, int posJ) : IdentityMatrix(r) {
        this->coefficient = coefficient;
        this->posI = posI;
        this->posJ = posJ;

        arr2[posI][posJ] = 0 - coefficient;
    }
};

class PermutationMatrix : public IdentityMatrix {
public:
    PermutationMatrix(int r, int row1, int row2) : IdentityMatrix(r) {
        arr2[row1][row1] = 0;
        arr2[row2][row2] = 0;
        arr2[row1][row2] = 1;
        arr2[row2][row1] = 1;
    }
};

Matrix getInverseMatrix(Matrix* matrix) {

    for (int i = 0; i < matrix->rows; i++) {

        int maxRow = i;
        double maxValue = matrix->arr2[maxRow][i];

        for (int j = i + 1; j < matrix->rows; j++) {
            if (abs(matrix->arr2[j][i]) > abs(maxValue)) {
                maxValue = matrix->arr2[j][i];
                maxRow = j;

            }
        }
        // permutation
        if (maxRow != i) {

            PermutationMatrix P = *new PermutationMatrix(matrix->rows, i, maxRow);
            matrix = (P * matrix);


        }


        // elimination
        for (int k = i + 1; k < matrix->rows; k++) {

            double f = matrix->arr2[k][i] / matrix->arr2[i][i];
            EliminationMatrix E = *new EliminationMatrix(matrix->rows, f, k, i);
            IdentityMatrix identity = *new IdentityMatrix(matrix->rows);
            SquareMatrix* e = &E;
            SquareMatrix* id = &identity;
            if (*e == *id) {
                break;
            } else {
                matrix = (E * matrix);
                matrix->arr2[k][i] = 0;


            }
        }
    }
    // Way back
    for (int i = matrix->rows - 1; i > -1; i--) {
        for (int j = i - 1; j > -1; j--) {
            double fa = matrix->arr2[j][i] / matrix->arr2[i][i];
            EliminationMatrix E = *new EliminationMatrix(matrix->rows, fa, j, i);
            IdentityMatrix identity = *new IdentityMatrix(matrix->rows);
            SquareMatrix* e = &E;
            SquareMatrix* id = &identity;
            if (*e == *id) {
                break;
            } else {

                matrix = (E * matrix);


            }
        }
    }
    // Diagonal normalization
    for (int i = 0; i < matrix->rows; i++) {
        double temp = matrix->arr2[i][i];
        for (int j = 0; j < matrix->columns; j++) {
            matrix->arr2[i][j] = (matrix->arr2[i][j] / temp);

        }
    }
    Matrix result = *new Matrix(matrix->rows, matrix->rows);

    for (int i = 0; i < matrix->rows; i++) {
        for (int j = matrix->rows; j < matrix->columns; j++) {
            result.arr2[i][j - matrix->rows] = matrix->arr2[i][j];
        }

    }

    return result;
}

Matrix augmented(int degree, Matrix* matrix) {
    Matrix A = *new Matrix(matrix->rows, degree + 1);

    for (int i = 0; i < matrix->rows; i++) {
        for (int j = 0; j < degree + 1; j++) {

            A.arr2[i][j] = pow(matrix->arr2[i][0], j);

        }
    }

    return A;
}

Matrix augmentedForInverse(SquareMatrix* matrix) {
    Matrix A = *new Matrix(matrix->rows, matrix->rows * 2);

    for (int i = 0; i < matrix->rows; i++) {
        for (int j = 0; j < matrix->columns; j++) {
            A.arr2[i][j] = matrix->arr2[i][j];
        }
    }

    for (int i = 0; i < A.rows; i++) {
        for (int j = matrix->columns; j < A.columns; j++) {
            if (j == i + matrix->rows)
                A.arr2[i][j] = 1;
            else
                A.arr2[i][j] = 0;
        }
    }
    return A;
}

Matrix* LSA(Matrix &dataT, Matrix &dataB, int degree) {

    Matrix A = augmented(degree, &dataT);
    Matrix* A_T_A = (*A.T() * &A);

    Matrix augmented = augmentedForInverse((SquareMatrix*) A_T_A);
    Matrix A_T_A_inv = getInverseMatrix(&augmented);

    Matrix variable =  *(*A.T() * &dataB);

    return (A_T_A_inv * &variable);
}

#ifdef WIN32
#define GNUPLOT_NAME "C:\\gnuplot\\bin\\gnuplot -persist"
#endif

int main() {

    #ifdef WIN32
        FILE* pipe = _popen(GNUPLOT_NAME, "w");
    #endif

    int r = 10;
    int degree = 3;
    Matrix t = *new Matrix(r, 1);
    Matrix b = *new Matrix(r, 1);

    t.arr2[0][0] = 1.03;
    t.arr2[1][0] = 1.11;
    t.arr2[2][0] = 2.02;
    t.arr2[3][0] = 4.25;
    t.arr2[4][0] = 5.25;
    t.arr2[5][0] = 9.03;
    t.arr2[6][0] = 9.31;
    t.arr2[7][0] = 10.08;
    t.arr2[8][0] = 11.01;
    t.arr2[9][0] = 11.04;

    b.arr2[0][0] = 11.14;
    b.arr2[1][0] = 11.18;
    b.arr2[2][0] = 11.05;
    b.arr2[3][0] = 9.02;
    b.arr2[4][0] = 7.07;
    b.arr2[5][0] = 12.23;
    b.arr2[6][0] = 9.23;
    b.arr2[7][0] = 1.26;
    b.arr2[8][0] = 1.25;
    b.arr2[9][0] = 4.04;

    Matrix* lsa = LSA(t, b, degree);

    fprintf(pipe, "set yrange [-10:30]\n");
    fprintf(pipe, "set xrange [-10:30]\n");

    fprintf(pipe, "plot %lf*x**3 + %lf*x**2 + %lf*x**1 + %lf*x**0 lc 'green', '-' with points pt 10 lc 'blue'\n", lsa->arr2[3][0], lsa->arr2[2][0], lsa->arr2[1][0], lsa->arr2[0][0]);

    for (int i = 0; i < 10 ; i++) {
        fprintf(pipe, "%f\t%f\n", t.arr2[i][0], b.arr2[i][0]);
    }

    fprintf(pipe, "e\n");
    fflush(pipe);
#ifdef WIN32
        _pclose(pipe);
    #endif

    return 0;
}

