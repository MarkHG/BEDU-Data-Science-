-- Ejercicios Sesión 2.
-- Todas las consultas que realices deberás mantenerlas dentro del editor de textos de MySQL Workbench. 
-- Al finalizar, guarda este archivo, llendo al menú File > Save script.

 USE classicmodels;
 DESCRIBE employees;

-- 1. Dentro de la tabla employees, obten el número de empleado, 
-- apellido y nombre de todos los empleados cuyo nombre empiece con A.
 SELECT employeeNumber, lastName, firstName
 FROM employees
 WHERE firstName LIKE 'A%';
 
-- 2.Dentro de la tabla employees, obten el número de empleado, apellido 
-- y nombre de todos los empleados cuyo apellido termina con on.
 SELECT employeeNumber, lastName, FirstName
 FROM employees
 WHERE lastName LIKE '%on';
 
-- 3.Dentro de la tabla employees, obten el número de empleado, apellido y nombre 
-- de todos los empleados cuyo nombre incluye la cadena on.
 SELECT employeeNumber, lastName, FirstName
 FROM employees
 WHERE firstName LIKE '%on%';
 
-- 4.Dentro de la tabla employees, obten el número de empleado, apellido y nombre 
-- de todos los empleados cuyos nombres tienen seis letras e inician con G.
 SELECT employeeNumber, lastName, firstName
 FROM employees
 WHERE firstName LIKE 'G_____';

-- 5.Dentro de la tabla employees, obten el número de empleado, 
-- apellido y nombre de todos los empleados cuyo nombre no inicia con B.
 SELECT employeeNumber, lastName, firstName
 FROM employees
 WHERE firstName NOT LIKE 'B%';
 
-- 6. Dentro de la tabla products, obten el código de producto 
-- y nombre de los productos cuyo código incluye la cadena _20.
 DESCRIBE products;
 SELECT productCode, productName
 FROM products
 WHERE productCode LIKE '%\_20%';

-- 7. Dentro de la tabla orderdetails, obten el total de cada orden.
 
 -- La idea que yo tenía inicialmente.
 DESCRIBE orderdetails;
 SELECT orderNumber, COUNT(*) AS total_por_orden
 FROM orderdetails
 GROUP BY orderNumber;
 
 /*
 Idea que me proporciono en Slack Dalai Aguirre "Tienes razón con la redacción del ejercicio, 
 a mí me parece que se refiere al precio de las órdenes"
 */
 SELECT SUM(priceEach), COUNT(*) AS cantidad_de_pagos
 FROM orderdetails
 GROUP BY priceEach;
 
-- 8. Dentro de la tabla orders obten el número de órdenes por año.
 DESCRIBE orders;
 SELECT YEAR(orderDate) AS año, COUNT(*) AS numero_orden
 FROM orders
 GROUP BY año;
 
-- 9. Obten el apellido y nombre de los empleados cuya oficina está ubicada en USA.
 SELECT lastName, firstName
 FROM employees
 WHERE officeCode IN(SELECT officeCode
  FROM offices
   WHERE country = 'USA');

-- 10.Obten el número de cliente, número de cheque y 
-- cantidad del cliente que ha realizado el pago más alto.
 DESCRIBE payments;
 SELECT customerNumber, checkNumber, (SELECT MAX(amount) 
  FROM payments)
 FROM payments
 GROUP BY customerNumber, checkNumber 
 LIMIT 1;
 
 
 







 


 