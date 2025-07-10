module Hydra.Ext.Demos.GenPG.Generated.DatabaseSchema where

import Hydra.Dsl.Tabular (TableType, tableType, columnType)
import Hydra.Dsl.Types (string, int32, float64, boolean)

appointmentsTable :: TableType
appointmentsTable = tableType "appointments.csv" [
  columnType "appointment_id" int32,
  columnType "patient_id" int32,
  columnType "doctor_id" int32,
  columnType "appointment_date" string,
  columnType "appointment_time" string,
  columnType "duration_minutes" int32,
  columnType "status" string,
  columnType "room_number" string,
  columnType "appointment_type" string,
  columnType "follow_up_required" boolean,
  columnType "notes" string]

billingTable :: TableType
billingTable = tableType "billing.csv" [
  columnType "billing_id" int32,
  columnType "patient_id" int32,
  columnType "appointment_id" int32,
  columnType "service_description" string,
  columnType "date_billed" string,
  columnType "amount" float64,
  columnType "insurance_coverage" float64,
  columnType "patient_responsibility" float64,
  columnType "payment_status" string,
  columnType "payment_date" string]

doctorsTable :: TableType
doctorsTable = tableType "doctors.csv" [
  columnType "doctor_id" int32,
  columnType "first_name" string,
  columnType "last_name" string,
  columnType "specialization" string,
  columnType "email" string,
  columnType "phone" string,
  columnType "hire_date" string,
  columnType "department_id" int32,
  columnType "license_number" string,
  columnType "is_resident" boolean,
  columnType "supervisor_id" int32]

medicalTestsTable :: TableType
medicalTestsTable = tableType "medical_tests.csv" [
  columnType "test_id" int32,
  columnType "patient_id" int32,
  columnType "doctor_id" int32,
  columnType "test_name" string,
  columnType "test_date" string,
  columnType "result_date" string,
  columnType "result_status" string,
  columnType "department_id" int32,
  columnType "technician_id" int32,
  columnType "priority" string,
  columnType "notes" string]

patientsTable :: TableType
patientsTable = tableType "patients.csv" [
  columnType "patient_id" int32,
  columnType "first_name" string,
  columnType "last_name" string,
  columnType "date_of_birth" string,
  columnType "gender" string,
  columnType "phone" string,
  columnType "email" string,
  columnType "address" string,
  columnType "insurance_provider" string,
  columnType "registration_date" string,
  columnType "primary_doctor_id" int32]

pharmacyTable :: TableType
pharmacyTable = tableType "pharmacy.csv" [
  columnType "pharmacy_id" int32,
  columnType "pharmacy_name" string,
  columnType "address" string,
  columnType "phone" string,
  columnType "email" string,
  columnType "hours_of_operation" string,
  columnType "is_hospital_affiliated" boolean,
  columnType "manager_name" string]

prescriptionsTable :: TableType
prescriptionsTable = tableType "prescriptions.csv" [
  columnType "prescription_id" int32,
  columnType "patient_id" int32,
  columnType "doctor_id" int32,
  columnType "medication_name" string,
  columnType "dosage" string,
  columnType "frequency" string,
  columnType "start_date" string,
  columnType "end_date" string,
  columnType "refill_count" int32,
  columnType "pharmacy_id" int32,
  columnType "is_controlled" boolean,
  columnType "notes" string]

techniciansTable :: TableType
techniciansTable = tableType "technicians.csv" [
  columnType "technician_id" int32,
  columnType "first_name" string,
  columnType "last_name" string,
  columnType "specialization" string,
  columnType "department_id" int32,
  columnType "hire_date" string,
  columnType "certification" string,
  columnType "phone" string,
  columnType "email" string,
  columnType "supervisor_id" int32]

departmentsTable :: TableType
departmentsTable = tableType "departments.csv" [
  columnType "department_id" int32,
  columnType "department_name" string,
  columnType "location" string,
  columnType "head_doctor_id" int32,
  columnType "founded_date" string,
  columnType "budget" float64,
  columnType "is_teaching" boolean,
  columnType "max_capacity" int32]

generatedTableSchemas :: [TableType]
generatedTableSchemas = [
  appointmentsTable,
  billingTable,
  doctorsTable,
  medicalTestsTable,
  patientsTable,
  pharmacyTable,
  prescriptionsTable,
  techniciansTable,
  departmentsTable]
