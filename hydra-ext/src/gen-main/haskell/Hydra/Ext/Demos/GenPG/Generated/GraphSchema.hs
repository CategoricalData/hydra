-- File: src/gen-main/haskell/Hydra.Ext.Demos/GenPG/Generated/GraphSchema.hs

module Hydra.Ext.Demos.GenPG.Generated.GraphSchema where

import Hydra.Ext.Dsl.Pg.Schemas (propertyType, required, schema, simpleEdgeType, vertexType)
import Hydra.Dsl.Types (string, int32, float64, boolean)

-- Reusable types
dateType = string
timeType = string
decimalType = float64

-- Vertex labels
patientVertex = "Patient"
doctorVertex = "Doctor"
technicianVertex = "Technician"
departmentVertex = "Department"
appointmentVertex = "Appointment"
medicalTestVertex = "MedicalTest"
prescriptionVertex = "Prescription"
pharmacyVertex = "Pharmacy"
billingVertex = "Billing"

-- Edge labels
supervisesEdge = "supervises"
belongsToEdge = "belongsTo"
referredByEdge = "referredBy"
assignedToEdge = "assignedTo"
conductedByEdge = "conductedBy"
prescribedByEdge = "prescribedBy"
filledByEdge = "filledBy"
headOfEdge = "headOf"
prescribedToEdge = "prescribedTo"
patientEdge = "patient"
appropriateForEdge = "appropriateFor"
paymentForEdge = "paymentFor"

-- Schema
generatedGraphSchema = schema vertexTypes edgeTypes where
  vertexTypes = [
    vertexType patientVertex int32 [
      required $ propertyType "firstName" string,
      required $ propertyType "lastName" string,
      propertyType "dateOfBirth" dateType,
      propertyType "gender" string,
      propertyType "phone" string,
      propertyType "email" string,
      propertyType "address" string,
      propertyType "insuranceProvider" string,
      propertyType "registrationDate" dateType],

    vertexType doctorVertex int32 [
      required $ propertyType "firstName" string,
      required $ propertyType "lastName" string,
      propertyType "specialization" string,
      propertyType "email" string,
      propertyType "phone" string,
      propertyType "hireDate" dateType,
      propertyType "licenseNumber" string,
      propertyType "isResident" boolean],

    vertexType technicianVertex int32 [
      required $ propertyType "firstName" string,
      required $ propertyType "lastName" string,
      propertyType "specialization" string,
      propertyType "hireDate" dateType,
      propertyType "certification" string,
      propertyType "phone" string,
      propertyType "email" string],

    vertexType departmentVertex int32 [
      required $ propertyType "name" string,
      propertyType "location" string,
      propertyType "foundedDate" dateType,
      propertyType "budget" decimalType,
      propertyType "isTeaching" boolean,
      propertyType "maxCapacity" int32],

    vertexType appointmentVertex int32 [
      required $ propertyType "appointmentDate" dateType,
      propertyType "appointmentTime" timeType,
      propertyType "durationMinutes" int32,
      propertyType "status" string,
      propertyType "roomNumber" string,
      propertyType "appointmentType" string,
      propertyType "followUpRequired" boolean,
      propertyType "notes" string],

    vertexType medicalTestVertex int32 [
      required $ propertyType "testName" string,
      propertyType "testDate" dateType,
      propertyType "resultDate" dateType,
      propertyType "resultStatus" string,
      propertyType "priority" string,
      propertyType "notes" string],

    vertexType prescriptionVertex int32 [
      required $ propertyType "medicationName" string,
      propertyType "dosage" string,
      propertyType "frequency" string,
      propertyType "startDate" dateType,
      propertyType "endDate" dateType,
      propertyType "refillCount" int32,
      propertyType "isControlled" boolean,
      propertyType "notes" string],

    vertexType pharmacyVertex int32 [
      required $ propertyType "name" string,
      propertyType "address" string,
      propertyType "phone" string,
      propertyType "email" string,
      propertyType "hoursOfOperation" string,
      propertyType "isHospitalAffiliated" boolean,
      propertyType "managerName" string],

    vertexType billingVertex int32 [
      propertyType "serviceDescription" string,
      propertyType "dateBilled" dateType,
      propertyType "amount" decimalType,
      propertyType "insuranceCoverage" decimalType,
      propertyType "patientResponsibility" decimalType,
      propertyType "paymentStatus" string,
      propertyType "paymentDate" dateType]
    ]

  edgeTypes = [
    simpleEdgeType supervisesEdge doctorVertex doctorVertex [],
    simpleEdgeType supervisesEdge doctorVertex technicianVertex [],
    simpleEdgeType belongsToEdge doctorVertex departmentVertex [],
    simpleEdgeType belongsToEdge technicianVertex departmentVertex [],
    simpleEdgeType headOfEdge doctorVertex departmentVertex [],
    simpleEdgeType referredByEdge patientVertex doctorVertex [],
    simpleEdgeType assignedToEdge appointmentVertex doctorVertex [],
    simpleEdgeType patientEdge appointmentVertex patientVertex [],
    simpleEdgeType conductedByEdge medicalTestVertex technicianVertex [],
    simpleEdgeType assignedToEdge medicalTestVertex doctorVertex [],
    simpleEdgeType patientEdge medicalTestVertex patientVertex [],
    simpleEdgeType prescribedByEdge prescriptionVertex doctorVertex [],
    simpleEdgeType prescribedToEdge prescriptionVertex patientVertex [],
    simpleEdgeType filledByEdge prescriptionVertex pharmacyVertex [],
    simpleEdgeType patientEdge billingVertex patientVertex [],
    simpleEdgeType paymentForEdge billingVertex appointmentVertex []]
