-- File: src/gen-main/haskell/Hydra.Ext.Demos/GenPG/Generated/Mapping.hs

module Hydra.Ext.Demos.GenPG.Generated.Mapping where

import Hydra.Core (Term)
import Hydra.Pg.Model (Vertex, Edge)
import Hydra.Formatting (decapitalize)
import Hydra.Phantoms (TTerm)
import Hydra.Dsl.Phantoms ((@@), constant, just, lambda, nothing, string, var)
import Hydra.Ext.Dsl.Pg.Mappings (LazyGraph, column, edgeNoId, graph, property, vertex)
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Lib.Strings as Strings
import Hydra.Ext.Demos.GenPG.Generated.GraphSchema

labeledIntId :: String -> TTerm (r -> Maybe Int) -> TTerm (r -> String)
labeledIntId itype iid = lambda "r" $ Maybes.map
  (lambda "i" $ Strings.concat [
    string $ decapitalize itype,
    string "_",
    Literals.showInt32 (var "i")])
  (iid @@ var "r")

generatedGraphMapping :: LazyGraph Term
generatedGraphMapping = graph

  -- Vertices
  [ vertex "patients.csv" patientVertex (labeledIntId patientVertex $ column "patient_id") [
      property "firstName" $ column "first_name",
      property "lastName" $ column "last_name",
      property "dateOfBirth" $ column "date_of_birth",
      property "gender" $ column "gender",
      property "phone" $ column "phone",
      property "email" $ column "email",
      property "address" $ column "address",
      property "insuranceProvider" $ column "insurance_provider",
      property "registrationDate" $ column "registration_date"],

    vertex "doctors.csv" doctorVertex (labeledIntId doctorVertex $ column "doctor_id") [
      property "firstName" $ column "first_name",
      property "lastName" $ column "last_name",
      property "specialization" $ column "specialization",
      property "email" $ column "email",
      property "phone" $ column "phone",
      property "hireDate" $ column "hire_date",
      property "licenseNumber" $ column "license_number",
      property "isResident" $ column "is_resident"],

    vertex "technicians.csv" technicianVertex (labeledIntId technicianVertex $ column "technician_id") [
      property "firstName" $ column "first_name",
      property "lastName" $ column "last_name",
      property "specialization" $ column "specialization",
      property "hireDate" $ column "hire_date",
      property "certification" $ column "certification",
      property "phone" $ column "phone",
      property "email" $ column "email"],

    vertex "departments.csv" departmentVertex (labeledIntId departmentVertex $ column "department_id") [
      property "name" $ column "department_name",
      property "location" $ column "location",
      property "foundedDate" $ column "founded_date",
      property "budget" $ column "budget",
      property "isTeaching" $ column "is_teaching",
      property "maxCapacity" $ column "max_capacity"],

    vertex "appointments.csv" appointmentVertex (labeledIntId appointmentVertex $ column "appointment_id") [
      property "appointmentDate" $ column "appointment_date",
      property "appointmentTime" $ column "appointment_time",
      property "durationMinutes" $ column "duration_minutes",
      property "status" $ column "status",
      property "roomNumber" $ column "room_number",
      property "appointmentType" $ column "appointment_type",
      property "followUpRequired" $ column "follow_up_required",
      property "notes" $ column "notes"],

    vertex "medical_tests.csv" medicalTestVertex (labeledIntId medicalTestVertex $ column "test_id") [
      property "testName" $ column "test_name",
      property "testDate" $ column "test_date",
      property "resultDate" $ column "result_date",
      property "resultStatus" $ column "result_status",
      property "priority" $ column "priority",
      property "notes" $ column "notes"],

    vertex "prescriptions.csv" prescriptionVertex (labeledIntId prescriptionVertex $ column "prescription_id") [
      property "medicationName" $ column "medication_name",
      property "dosage" $ column "dosage",
      property "frequency" $ column "frequency",
      property "startDate" $ column "start_date",
      property "endDate" $ column "end_date",
      property "refillCount" $ column "refill_count",
      property "isControlled" $ column "is_controlled",
      property "notes" $ column "notes"],

    vertex "pharmacy.csv" pharmacyVertex (labeledIntId pharmacyVertex $ column "pharmacy_id") [
      property "name" $ column "pharmacy_name",
      property "address" $ column "address",
      property "phone" $ column "phone",
      property "email" $ column "email",
      property "hoursOfOperation" $ column "hours_of_operation",
      property "isHospitalAffiliated" $ column "is_hospital_affiliated",
      property "managerName" $ column "manager_name"],

    vertex "billing.csv" billingVertex (labeledIntId billingVertex $ column "billing_id") [
      property "serviceDescription" $ column "service_description",
      property "dateBilled" $ column "date_billed",
      property "amount" $ column "amount",
      property "insuranceCoverage" $ column "insurance_coverage",
      property "patientResponsibility" $ column "patient_responsibility",
      property "paymentStatus" $ column "payment_status",
      property "paymentDate" $ column "payment_date"]
  ]

  -- Edges
  [ edgeNoId "doctors.csv" supervisesEdge
      (labeledIntId doctorVertex $ column "supervisor_id")
      (labeledIntId doctorVertex $ column "doctor_id")
      [],

    edgeNoId "technicians.csv" supervisesEdge
      (labeledIntId doctorVertex $ column "supervisor_id")
      (labeledIntId technicianVertex $ column "technician_id")
      [],

    edgeNoId "doctors.csv" belongsToEdge
      (labeledIntId doctorVertex $ column "doctor_id")
      (labeledIntId departmentVertex $ column "department_id")
      [],

    edgeNoId "technicians.csv" belongsToEdge
      (labeledIntId technicianVertex $ column "technician_id")
      (labeledIntId departmentVertex $ column "department_id")
      [],

    edgeNoId "departments.csv" headOfEdge
      (labeledIntId doctorVertex $ column "head_doctor_id")
      (labeledIntId departmentVertex $ column "department_id")
      [],

    edgeNoId "patients.csv" referredByEdge
      (labeledIntId patientVertex $ column "patient_id")
      (labeledIntId doctorVertex $ column "primary_doctor_id")
      [],

    edgeNoId "appointments.csv" assignedToEdge
      (labeledIntId appointmentVertex $ column "appointment_id")
      (labeledIntId doctorVertex $ column "doctor_id")
      [],

    edgeNoId "appointments.csv" patientEdge
      (labeledIntId appointmentVertex $ column "appointment_id")
      (labeledIntId patientVertex $ column "patient_id")
      [],

    edgeNoId "medical_tests.csv" assignedToEdge
      (labeledIntId medicalTestVertex $ column "test_id")
      (labeledIntId doctorVertex $ column "doctor_id")
      [],

    edgeNoId "medical_tests.csv" conductedByEdge
      (labeledIntId medicalTestVertex $ column "test_id")
      (labeledIntId technicianVertex $ column "technician_id")
      [],

    edgeNoId "medical_tests.csv" patientEdge
      (labeledIntId medicalTestVertex $ column "test_id")
      (labeledIntId patientVertex $ column "patient_id")
      [],

    edgeNoId "prescriptions.csv" prescribedByEdge
      (labeledIntId prescriptionVertex $ column "prescription_id")
      (labeledIntId doctorVertex $ column "doctor_id")
      [],

    edgeNoId "prescriptions.csv" prescribedToEdge
      (labeledIntId prescriptionVertex $ column "prescription_id")
      (labeledIntId patientVertex $ column "patient_id")
      [],

    edgeNoId "prescriptions.csv" filledByEdge
      (labeledIntId prescriptionVertex $ column "prescription_id")
      (labeledIntId pharmacyVertex $ column "pharmacy_id")
      [],

    edgeNoId "billing.csv" patientEdge
      (labeledIntId billingVertex $ column "billing_id")
      (labeledIntId patientVertex $ column "patient_id")
      [],

    edgeNoId "billing.csv" paymentForEdge
      (labeledIntId billingVertex $ column "billing_id")
      (labeledIntId appointmentVertex $ column "appointment_id")
      []
  ]
