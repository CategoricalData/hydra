module Hydra.Ext.Demos.GenPG.ExampleDatabaseSchema where

import Hydra.Tabular
import Hydra.Dsl.Tabular (tableType, columnType)
import Hydra.Dsl.Types (binary, boolean, float32, float64, int32, int64, string)

employeesTableType :: TableType
employeesTableType = tableType "employees.csv" [
  columnType "employee_id" int32,
  columnType "first_name" string,
  columnType "last_name" string,
  columnType "email" string,
  columnType "hire_date" string,
  columnType "manager_id" int32,
  columnType "department_id" int32,
  columnType "management_since_date" string,
  columnType "department_join_date" string,
  columnType "department_role" string]

departmentsTableType :: TableType
departmentsTableType = tableType "departments.csv" [
  columnType "department_id" int32,
  columnType "department_name" string,
  columnType "parent_department_id" int32,
  columnType "parent_department_since" string]

customersTableType :: TableType
customersTableType = tableType "customers.csv" [
  columnType "customer_id" int32,
  columnType "company_name" string,
  columnType "contact_name" string,
  columnType "email" string,
  columnType "phone" string]

productsTableType :: TableType
productsTableType = tableType "products.csv" [
  columnType "product_id" int32,
  columnType "product_name" string,
  columnType "price" float64,
  columnType "category_name" string,
  columnType "since_date" string,
  columnType "is_primary" boolean]

salesTableType :: TableType
salesTableType = tableType "sales.csv" [
  columnType "sale_id" int32,
  columnType "employee_id" int32,
  columnType "customer_id" int32,
  columnType "sale_date" string,
  columnType "total_amount" float64,
  columnType "commission_rate" float64,
  columnType "sales_channel" string,
  columnType "payment_method" string,
  columnType "satisfaction_rating" int32,
  columnType "is_repeat_customer" boolean]

saleItemsTableType :: TableType
saleItemsTableType = tableType "sale_items.csv" [
  columnType "sale_item_id" int32,
  columnType "sale_id" int32,
  columnType "product_id" int32,
  columnType "quantity" int32,
  columnType "item_price" float64,
  columnType "item_order" int32,
  columnType "discount_applied" float64,
  columnType "warranty_period" int32]

callsTableType :: TableType
callsTableType = tableType "calls.csv" [
  columnType "id" int32,
  columnType "customer_id" int32,
  columnType "employee_id" int32,
  columnType "interaction_date" string,
  columnType "notes" string,
  columnType "duration_minutes" int32,
  columnType "follow_up_required" boolean,
  columnType "customer_initiated" boolean]

emailsTableType :: TableType
emailsTableType = tableType "emails.csv" [
  columnType "id" int32,
  columnType "customer_id" int32,
  columnType "employee_id" int32,
  columnType "interaction_date" string,
  columnType "notes" string,
  columnType "duration_minutes" int32,
  columnType "follow_up_required" boolean,
  columnType "customer_initiated" boolean]

meetingsTableType :: TableType
meetingsTableType = tableType "meetings.csv" [
  columnType "id" int32,
  columnType "customer_id" int32,
  columnType "employee_id" int32,
  columnType "interaction_date" string,
  columnType "notes" string,
  columnType "duration_minutes" int32,
  columnType "follow_up_required" boolean,
  columnType "customer_initiated" boolean]

salesTableSchemas :: [TableType]
salesTableSchemas = [
  employeesTableType,
  departmentsTableType,
  customersTableType,
  productsTableType,
  salesTableType,
  saleItemsTableType,
  callsTableType,
  emailsTableType,
  meetingsTableType]
